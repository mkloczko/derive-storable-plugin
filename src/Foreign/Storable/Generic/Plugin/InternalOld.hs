{-# LANGUAGE PatternGuards #-}
module Foreign.Storable.Generic.Plugin.Internal where

import CoreSyn (Bind(..),Expr(..), CoreExpr, CoreBind, CoreProgram, Alt)
import Literal (Literal(..))
import Id  (isLocalId, isGlobalId,Id)
import Var (Var(..))
import Name (getOccName,mkOccName, getSrcSpan)
import qualified Name as N (varName)
import SrcLoc (SrcSpan,noSrcSpan)
import Unique (getUnique)
-- import PrelNames (intDataConKey)
-- import FastString (mkFastString)
-- import TysPrim (intPrimTy)
import HscMain (hscCompileCoreExpr)
import HscTypes (HscEnv)
import TyCoRep (Type(..), TyBinder(..))
import TysWiredIn (intDataCon)
import DataCon    (dataConWorkId) 
import Outputable (cat, ppr, SDoc, showSDocUnsafe)
import GHCi.RemoteTypes  
import MkCore (mkWildValBinder)
import Unsafe.Coerce

import Data.List
import Debug.Trace
import Control.Monad.IO.Class

data ErrorMsg = OrderingFailed Int [CoreBind] | Other String

-- | Predicate used to find gsizeOf identifiers
-- Note to self - perhaps export OccName.NameSpace constructors ?
-- varName is just a wrapper over VarName constructor..
isSizeOfId :: CoreBind -> Bool
isSizeOfId (NonRec ident _) = getOccName (varName ident)    == mkOccName N.varName "$cgsizeOf" 
isSizeOfId _                = False -- "We expect gsizeOf to not be defined recursively.

-- | Predicate used to find galignment identifiers
isAlignmentId :: CoreBind -> Bool
isAlignmentId (NonRec ident _) = getOccName (varName ident) == mkOccName N.varName "$cgalignment" 
isAlignmentId _                = False -- "We expect galignment to not be defined recursively.

-- | Predicate used to find gpeekByteOff identifiers
isPeekId :: CoreBind -> Bool
isPeekId (NonRec ident _) = getOccName (varName ident) == mkOccName N.varName "$cgpeekByteOff" 
isPeekId _                = False -- "We expect galignment to not be defined recursively.

-- | Takes the gsizeOf and galignment bindings from the Core program.
paritionBasicBinds :: CoreProgram -> ([CoreBind],[CoreBind])
paritionBasicBinds core_prog = partition (\bind -> isSizeOfId bind || isAlignmentId bind) core_prog

---------------------------------------------------
-- Note [Order of optimisation for nested types] --
---------------------------------------------------
--
-- It is possible for the user to define nested instances
-- in the same module with their components. For example:
-- module Example where
--
--  
-- data Flat   = Flat   Int deriving (Generic, GStorable)
-- data Nested = Nested A   deriving (Generic, GStorable)
--
-- We would like to make sure that we calculate gsizeOf ang galignment first for
-- non nested types (components from other modules are allowed in this category).
-- Then we go up in hierarchy and calculate for the nested ones.

-- add from to f names
-- | Get ids from core bind.
getIdsBind :: CoreBind -> [Id]
getIdsBind (NonRec id _) = [id]
getIdsBind (Rec recs)    = map fst recs

-- | Get all expressions from a binding.
getExprsBind :: CoreBind -> [CoreExpr]
getExprsBind (NonRec _ e) = [e]
getExprsBind (Rec   recs) = map snd recs

getIdsExprsBind :: CoreBind -> [(Id,CoreExpr)]
getIdsExprsBind (NonRec id expr) = [(id,expr)]
getIdsExprsBind (Rec       recs) = recs

-- | Get all IDs from CoreExpr
getIdsExpr :: CoreExpr -> [Id]
getIdsExpr (Var id)    = [id]
getIdsExpr (App e1 e2) = concat [getIdsExpr e1, getIdsExpr e2]
getIdsExpr (Lam id e)  = id : getIdsExpr e
-- Ids from bs are ignored, as they are supposed to appear in e argument.
getIdsExpr (Let bs e)  = concat [getIdsExpr e, concatMap getIdsExpr (getExprsBind bs)]
-- The case_binder is ignored - the evaluated expression might appear on the rhs of alts
getIdsExpr (Case e _ _ alts) = concat $ getIdsExpr e : map (\(_,_,e_c) -> getIdsExpr e_c) alts
getIdsExpr (Cast e _) = getIdsExpr e 
getIdsExpr _           = []




-- Possible optimisation - remove the check for own id.
-- Change name to nested
isNested :: [Id]     -- ^ List of possible local components
         -> CoreBind -- ^ The binding itself  
         -> Bool     -- ^ Result
isNested ids core_bind = do
    let my_ids   = getIdsBind core_bind
        expr_ids = concatMap getIdsExpr $ getExprsBind core_bind
    any (\e_id -> (e_id `elem` ids) && (e_id `notElem` my_ids)) expr_ids   





-- | Organises the selected bindings into a hierarchy.
-- See note [Organising into a hierarchy]
-- Compilation should happen while the hierarchy gets ordered.
orderByNested :: [CoreBind]   -- ^ The gsizeOf and galignemnt binds
           -- -> [CoreBind]   -- ^ The rest of the CoreProgram bindings
              -> ([[CoreBind]], Maybe ErrorMsg) -- ^ The hierarchy, starting from non nested.
orderByNested g_binds {- other_binds -} = do
    -- Find the non_nested ones.
    let ids = concatMap getIdsBind g_binds
        predicate = not.(isNested ids)
        (non_nested,rest) = partition predicate g_binds
        -- The ordering itself
        (ordering, m_err) = orderByNested_rec rest [non_nested] 
    (reverse ordering, m_err) 

orderByNested_rec :: [CoreBind]   -- ^ Bindings to order 
               -- -> [CoreBind]   -- ^ All other bindings except gsizeOf and galignment
                  -> [[CoreBind]] -- ^ The accumulator with ordered bindings
                  -> ([[CoreBind]], Maybe ErrorMsg) -- ^ The end result, starting from the most nested.
orderByNested_rec []                        acc = (acc, Nothing) -- all got sorted.
orderByNested_rec g_binds {- other_binds -} acc = do
    let ids               = concatMap getIdsBind g_binds
        predicate         = not.(isNested ids)
        (new_layer, rest) = partition predicate g_binds

        -- Checking whether something went wrong. 
    if length new_layer == 0 then do
            -- Put the unordered elements into error msg.
            -- Perhaps interpret them later on, and inform
            -- what did go wrong?
        let error_msg = OrderingFailed (length acc) rest
        (acc, Just error_msg)
    else orderByNested_rec rest (new_layer:acc) 


-- We got hierarchy. noow..

compileExpr :: HscEnv -> CoreExpr -> SrcSpan -> IO a 
compileExpr hsc_env expr src_span = do
    foreign_hval <- liftIO $ hscCompileCoreExpr hsc_env src_span expr
    hval         <- liftIO $ withForeignRef foreign_hval localRef
    let val = unsafeCoerce hval :: a 
    -- finalizeForeignRef foreign_hval  -- check whether that's the source of the error
    return val

intToExpr :: Type -> Int -> CoreExpr
intToExpr t i = Lam wild $ App fun arg
    where fun = Var $ dataConWorkId intDataCon
          arg = Lit $ MachInt $ fromIntegral i
          wild= mkWildValBinder t 
-- | Get the 'a' from 'a -> b' types.
getFirstArgument :: Id -> Maybe Type
getFirstArgument id
    | (ForAllTy tb t) <- varType id
    , Anon t_ok <- tb
    = Just t_ok
    | otherwise = Nothing

isWhat :: Type -> String
isWhat (AppTy t1 _) = "app ty " ++ showSDocUnsafe (ppr t1)
isWhat (TyVarTy t ) = "tyvarty " ++ showSDocUnsafe (ppr t)
isWhat (TyConApp t ts ) = "tyconapp " ++ showSDocUnsafe (cat [ppr t, ppr ts] )
isWhat (ForAllTy tb t ) = "forallTy " ++ showSDocUnsafe (cat$ [ppr tb,ppr t])
isWhat (LitTy t ) = "LitTy " ++ showSDocUnsafe (ppr t)
isWhat otherwise  = "bla"
-- | Assumes that the expression is of Lambda app 
--
intSubstitution :: HscEnv -> CoreBind -> IO CoreBind
intSubstitution hsc_env e@(Rec    _) = trace "Recursive" $ return e
intSubstitution hsc_env e@(NonRec id (Lam _ (Lam _ _))) = trace "too much lambdas.." $ return e 
intSubstitution hsc_env e@(NonRec id (Lam _ expr)) = do
    -- putStrLn "Trying to compile something..."
    -- putStrLn $ showSDocUnsafe $ ppr e 
    the_integer <- compileExpr hsc_env expr (getSrcSpan id) :: IO Int
    -- putStrLn $ "Got expression: " ++ (showSDocUnsafe $ cat [ppr $ getIdsBind e, ppr $ map varType (getIdsBind e)])
    -- putStrLn $ "With type " ++ (showSDocUnsafe $ cat [ppr $ varType id])
    -- putStrLn $ "typetyp " ++ ((isWhat.varType) id)
    -- putStrLn $ showSDocUnsafe $ ppr e
    let m_t      = getFirstArgument id 
    case m_t of
        Just t -> do
            -- putStrLn $ "Now it is: " ++ show the_integer 
            -- putStrLn $ showSDocUnsafe $ ppr (NonRec id (intToExpr t the_integer))
            -- putStrLn $ ""
            return $ NonRec id (intToExpr t the_integer)
        Nothing -> return e
-- | Substitutes the localIds inside the expression with bodies of provided bindings.
-- exprSubstitution :: [CoreBind]  -- ^ Provided bindings.
--                  -> CoreBind    -- ^ The binding which will be transformed
--                  -> CoreBind    -- ^ Transformed result.
-- exprSubstitution provided_binds the_bind = 

-- There's a problem with recursive bindings.

replaceIdsBind :: [CoreBind] -> CoreBind -> CoreBind
replaceIdsBind prov_bs (NonRec id e) = NonRec id (replaceIds prov_bs e)
replaceIdsBind prov_bs (Rec    recs) = Rec $ map (\(id,e) -> (id,replaceIds prov_bs e)) recs

replaceIds :: [CoreBind] -> CoreExpr -> CoreExpr
replaceIds prov_bs e@(Var id)
    | isLocalId id
    , Just (_,expr) <- find ((id==).fst) $ concatMap getIdsExprsBind prov_bs -- if is recursive - remove it from prov_bs
    = replaceIds prov_bs expr
    | otherwise = e    
replaceIds prov_bs (App e1 e2) = App (replaceIds prov_bs e1) (replaceIds prov_bs e2)
replaceIds prov_bs (Lam id e)  = Lam id (replaceIds prov_bs e)
replaceIds prov_bs (Let  b e)  = Let (replaceIdsBind prov_bs b) (replaceIds prov_bs e)
replaceIds prov_bs (Case e ev t alts) = do
    let new_e = replaceIds prov_bs e
        new_alts = map (\(alt, ids, exprs) -> (alt,ids, replaceIds prov_bs exprs)) alts
    Case new_e ev t new_alts
replaceIds prov_bs (Cast e c) = Cast (replaceIds prov_bs e) c
replaceIds prov_bs (Tick t e) = Tick t (replaceIds prov_bs e)
replaceIds prov_bs e          = e

---------------
-- The thing --
---------------

forceCompileTimeEvaluation :: HscEnv -> CoreProgram -> IO CoreProgram
forceCompileTimeEvaluation hsc_env core_prog = do
    let (gmethods, rest) = paritionBasicBinds core_prog
        (grouped, e_msg) = orderByNested gmethods
        err_rest = case e_msg of 
            Just (OrderingFailed _ bs) -> bs
            Nothing -> []
    case e_msg of
        Just (OrderingFailed n bs) -> do
            putStrLn $ "Ordering of nested instances has failed at level " ++ show n ++ "."
            putStrLn $ showSDocUnsafe$ ppr $ bs
        Nothing -> return ()
    -- Do while grouped are not empty
    --
    putStrLn $ "Found methods: " ++ showSDocUnsafe (ppr $ map getIdsBind gmethods)
    putStrLn $ "With uniques: "  ++ showSDocUnsafe (ppr $ map ((map getUnique).getIdsBind) gmethods)
    let mapped = map (map (\b -> (getIdsBind b, map varType (getIdsBind b) ) ) ) grouped
    putStrLn $ "Got groups:\n"    ++ concatMap (\x -> (showSDocUnsafe.ppr) x ++ "\n") mapped
    --
    compiled <- compileGroups hsc_env grouped rest
    return $ concat [compiled, err_rest, rest]

compileGroups :: HscEnv
              -> [[CoreBind]]  -- ^ Grouped bindings
              -> [CoreBind]    -- ^ Rest of local bindings
              -> IO [CoreBind] -- ^ Replaced bindings
compileGroups hsc_env [] rest = return []
compileGroups hsc_env (non_nested:gs) rest = do
    -- Replace any local ids in the expressions.
    let non_nested' = map (replaceIdsBind rest) non_nested
    -- Compile and substitute. Should catch errors here.
    compiled <- mapM (intSubstitution hsc_env) non_nested'
    compileGroups_rec hsc_env gs rest compiled 
    
compileGroups_rec :: HscEnv        
                  -> [[CoreBind]]  -- ^ Grouped bindings
                  -> [CoreBind]    -- ^ Rest of local bindings
                  -> [CoreBind]    -- ^ Already replaced
                  -> IO [CoreBind] -- ^ Replaced bindings
compileGroups_rec hsc_env []     rest acc = return acc 
compileGroups_rec hsc_env (g:gs) rest acc = do
    let replaced_locals  = map (replaceIdsBind rest) g
        replaced_glocals = map (replaceIdsBind acc)  replaced_locals
    compiled <- mapM (intSubstitution hsc_env) replaced_glocals
    compileGroups_rec hsc_env gs rest (concat [compiled, acc])


