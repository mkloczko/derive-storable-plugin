module Foreign.Storable.Generic.Plugin.Internal.Compile where

-- Management of Core.
import CoreSyn (Bind(..),Expr(..), CoreExpr, CoreBind, CoreProgram, Alt, AltCon(..))
import Literal (Literal(..))
import Id  (isLocalId, isGlobalId,Id)
import Var (Var(..))
import Name (getOccName,mkOccName, getSrcSpan)
import OccName (OccName(..), occNameString)
import qualified Name as N (varName)
import SrcLoc (noSrcSpan, SrcSpan)
import Unique (getUnique)
-- import PrelNames (intDataConKey)
-- import FastString (mkFastString)
-- import TysPrim (intPrimTy)
-- Compilation pipeline stuff
import HscMain (hscCompileCoreExpr)
import HscTypes (HscEnv,ModGuts(..))
import CoreMonad (CoreM, SimplifierMode(..),CoreToDo(..), getHscEnv)
import BasicTypes (CompilerPhase(..))
-- Haskell types 
import Type (isAlgType, splitTyConApp_maybe)
import TyCon (algTyConRhs, visibleDataCons)
import TyCoRep (Type(..), TyBinder(..))
import TysWiredIn (intDataCon)
import DataCon    (dataConWorkId,dataConOrigArgTys) 

import MkCore (mkWildValBinder)
-- Printing
import Outputable (cat, ppr, SDoc, showSDocUnsafe)
import Outputable (($$), ($+$), vcat, empty,text, (<>), (<+>), nest, int) 
import CoreMonad (putMsg, putMsgS)

-- Used to get to compiled values
import GHCi.RemoteTypes



import Unsafe.Coerce

import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace
import Control.Monad.IO.Class
import Control.Monad 

import Control.Exception

import Foreign.Storable.Generic.Plugin.Internal.Helpers
import Foreign.Storable.Generic.Plugin.Internal.Error
import Foreign.Storable.Generic.Plugin.Internal.Predicates
import Foreign.Storable.Generic.Plugin.Internal.Types


compileExpr :: HscEnv -> CoreExpr -> SrcSpan -> IO a 
compileExpr hsc_env expr src_span = do
    foreign_hval <- liftIO $ hscCompileCoreExpr hsc_env src_span expr
    hval         <- liftIO $ withForeignRef foreign_hval localRef
    let val = unsafeCoerce hval :: a 
    -- finalizeForeignRef foreign_hval  -- check whether that's the source of the error
    return val

tryCompileExpr :: Id -> CoreExpr -> CoreM (Either Error a)
tryCompileExpr id core_expr  = do
    hsc_env <- getHscEnv
    e_compiled <- liftIO $ try $ compileExpr hsc_env core_expr (getSrcSpan id)
    case e_compiled of
        Left  _  -> return $ Left $ CompilationError (NonRec id core_expr)
        Right val-> return $ Right val

----------------------
-- Int substitution --
----------------------

intToExpr :: Type -> Int -> CoreExpr
intToExpr t i = Lam wild $ App fun arg
    where fun = Var $ dataConWorkId intDataCon
          arg = Lit $ MachInt $ fromIntegral i
          wild= mkWildValBinder t 

intSubstitution :: CoreBind -> CoreM (Either Error CoreBind)
intSubstitution e@(Rec    _) = return $ Left $ CompilationNotSupported e
intSubstitution e@(NonRec id (Lam _ (Lam _ _))) = return $ Left $ CompilationNotSupported e 
intSubstitution e@(NonRec id (Lam _ expr)) = do
    -- Get HscEnv
    hsc_env     <- getHscEnv
    -- Try the subtitution.
    the_integer <- tryCompileExpr id expr :: CoreM (Either Error Int)
    -- Get the type.
    let m_t      = getGStorableType (varType id)
    case m_t of
        Just t ->  return $ NonRec id <$> (intToExpr t <$> the_integer)
        -- If the compilation error occured, first return it.
        Nothing -> 
            return the_integer >> return $ Left $ CompilationError e

-----------------------
-- peek substitution --
-----------------------

offsetSubstitution :: CoreBind -> CoreM (Either Error CoreBind)
offsetSubstitution b@(Rec _) = return $ Left $ CompilationNotSupported b
offsetSubstitution b = do
    return $ Left $ CompilationNotSupported b

-- | The data which can be expressed.
data OffsetScope = IntList Id CoreExpr
                 | IntVal  Id CoreExpr

-- | ToDo
intListExpr :: [Int] -> CoreExpr
intListExpr list = undefined 

exprToIntList :: Id -> CoreExpr -> CoreM (Either Error OffsetScope)
exprToIntList id core_expr = do
    int_list <- tryCompileExpr id core_expr
    let new_expr = intListExpr <$> int_list
    return $ IntList id <$> new_expr

-- | ToDo
intValExpr :: [Int] -> CoreExpr
intValExpr list = undefined

exprToIntVal :: Id -> CoreExpr -> CoreM (Either Error OffsetScope)
exprToIntVal id core_expr = do
    int_val <- tryCompileExpr id core_expr
    let new_expr = intValExpr <$> int_val
    return $ IntVal id <$> new_expr




offsetSubstitutionTree :: [OffsetScope] -> CoreExpr -> CoreM (Either Error CoreExpr)
offsetSubstitutionTree scope e@(Var  _)   = return $ Right e
offsetSubstitutionTree scope e@(Cast _ _) = return $ Right e
offsetSubstitutionTree scope e@(Tick _ _) = return $ Right e
offsetSubstitutionTree scope e@(Type _)   = return $ Right e
offsetSubstitutionTree scope expr
    | Let offset_bind case_expr <- expr
    , NonRec offset_id offset_expr <- offset_bind
    , isOffsetsId offset_id
    = do 
      e_new_s <- exprToIntList offset_id offset_expr 
      case e_new_s of
          Left err       -> return $ Left err
          Right int_list -> offsetSubstitutionTree (int_list:scope) case_expr
    | Case case_expr _ _ [alt0] <- expr
    , (DataAlt i_prim_con, [x_id], alt_expr) <- alt0
    , False
    = do 
      e_new_s <- exprToIntVal x_id case_expr 
      case e_new_s of
          Left err       -> return $ Left err
          Right int_list -> offsetSubstitutionTree (int_list:scope) alt_expr
-----------------
-- compilation --

-- | Compile the expression in Core Bind and replace it.
compileGStorableBind :: CoreBind -> CoreM (Either Error CoreBind) 
compileGStorableBind core_bind
    -- Substitute gsizeOf
    | (NonRec id expr) <- core_bind
    , isSizeOfId id 
    = intSubstitution core_bind
    -- Substitute galignment
    | (NonRec id expr) <- core_bind
    , isAlignmentId id  
    = intSubstitution core_bind
    -- TODO: Substitute peek binds
    | (NonRec id expr) <- core_bind
    , isPeekId id
    = offsetSubstitution core_bind
    -- TODO: Substitute poke binds
    | (NonRec id expr) <- core_bind
    , isPokeId id
    = return $ Right core_bind
    -- Everything else - nope.
    -- Perhaps warn/crash.
    | otherwise = return $ Left $ CompilationNotSupported core_bind



-- This part could use some optimizations, perhaps.
-- And refactoring... It's hard to read.

-- | Substitutes the localIds inside the expression with bodies of provided bindings, if possible.
replaceIdsBind :: [CoreBind] -- ^ Replace with - for GStorable bindings
               -> [CoreBind] -- ^ Replace with - for other top-bindings
               -> CoreBind   -- ^ Binding which will have ids replaced.
               -> CoreBind   -- ^ Binding with replaced ids.
replaceIdsBind gstorable_bs other_bs (NonRec id e) = NonRec id (replaceIds gstorable_bs other_bs e)
replaceIdsBind gstorable_bs other_bs (Rec    recs) = Rec $ map (\(id,e) -> (id,replaceIds gstorable_bs other_bs e)) recs

replaceIds :: [CoreBind] -- ^ Replace with - for GStorable bindins
           -> [CoreBind] -- ^ Replace with - for other top-bindings
           -> CoreExpr   -- ^ Expression which will have ids replaced.
           -> CoreExpr   -- ^ Expression with replaced ids.
replaceIds gstorable_bs other_bs e@(Var id)
    -- For non recs: GStorable and other.
    | isLocalId id
    , Just (_,expr) <- find ((id==).fst) $ [(id,expr) | NonRec id expr <- gstorable_bs]
    = replaceIds gstorable_bs other_bs expr
    | isLocalId id
    , Just (_,expr) <- find ((id==).fst) $ [(id,expr) | NonRec id expr <- other_bs]
    = replaceIds gstorable_bs other_bs expr
    -- For recs. The substituted component has to be removed.
    | isLocalId id
    , ([id_here],rest) <- partition (\x -> id `elem` (map fst x)) $ [bs | Rec bs <- gstorable_bs] 
    , Just (_,expr) <- find ((id==).fst) id_here
    = replaceIds (map Rec rest) other_bs expr
    | isLocalId id
    , ([id_here],rest) <- partition (\x -> id `elem` (map fst x)) $ [bs | Rec bs <- other_bs] 
    , Just (_,expr) <- find ((id==).fst) id_here
    = replaceIds gstorable_bs (map Rec rest) expr
    -- If is a global id, or id was not found (local inside the expression) - leave it alone.
    | otherwise = e
replaceIds gstorable_bs other_bs (App e1 e2) = App (replaceIds gstorable_bs other_bs e1) (replaceIds gstorable_bs other_bs e2)
replaceIds gstorable_bs other_bs (Lam id e)  = Lam id (replaceIds gstorable_bs other_bs e)
replaceIds gstorable_bs other_bs (Let  b e)  = Let (replaceIdsBind gstorable_bs other_bs b) (replaceIds gstorable_bs other_bs e)
replaceIds gstorable_bs other_bs (Case e ev t alts) = do
    let new_e = replaceIds gstorable_bs other_bs e
        new_alts = map (\(alt, ids, exprs) -> (alt,ids, replaceIds gstorable_bs other_bs exprs)) alts
    Case new_e ev t new_alts
replaceIds gstorable_bs other_bs (Cast e c) = Cast (replaceIds gstorable_bs other_bs e) c
replaceIds gstorable_bs other_bs (Tick t e) = Tick t (replaceIds gstorable_bs other_bs e)
replaceIds gstorable_bs other_bs e          = e


compileGroups :: Flags 
              -> [[CoreBind]] 
              -> [CoreBind] 
              -> CoreM [CoreBind]
compileGroups flags bind_groups bind_rest = compileGroups_rec flags 0 bind_groups bind_rest [] []


compileGroups_rec :: Flags
                  -> Int -- ^ Depth, usefull for debugging.
                  -> [[CoreBind]] 
                  -> [CoreBind] 
                  -> [CoreBind] 
                  -> [CoreBind] 
                  -> CoreM [CoreBind]
compileGroups_rec flags _ []       bind_rest subs not_subs = return $ concat [subs,not_subs]
compileGroups_rec flags d (bg:bgs) bind_rest subs not_subs = do
    let layer_replaced = map (replaceIdsBind bind_rest subs) bg
    e_compiled <- mapM compileGStorableBind layer_replaced
    let errors = lefts e_compiled
        compiled = rights e_compiled
    not_compiled <- compileGroups_error flags d errors
    compileGroups_rec flags (d+1) bgs bind_rest (concat [compiled,subs]) (concat [not_compiled, not_subs])

compileGroups_error :: Flags -> Int -> [Error] -> CoreM [CoreBind]
compileGroups_error flags d errors = do
   let (Flags verb to_crash) = flags
       crasher errs = case errs of
           []   -> return ()
           _    -> error "Crashing..."
       print_header txt = case verb of
           None  -> empty
           other ->    text "Errors while compiling and substituting bindings at depth " <+> int d <> text":" 
                    $$ nest 5 txt 
       printer errs = case errs of
           [] -> return ()
           ls ->  putMsg $ print_header (vcat (map (pprError verb) errs)) 
       ungroup err = case err of
           (CompilationNotSupported bind) -> bind
           (CompilationError        bind) -> bind
   printer errors
   when to_crash $ crasher errors
   return $ map ungroup errors
