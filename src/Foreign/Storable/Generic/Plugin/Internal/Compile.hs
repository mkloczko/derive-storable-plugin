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
import CoreLint (lintExpr)
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
import Outputable (Outputable(..),($$), ($+$), vcat, empty,text, (<>), (<+>), nest, int) 
import CoreMonad (putMsg, putMsgS)

-- Used to get to compiled values
import GHCi.RemoteTypes

-- Used to create types
import TysWiredIn
import DataCon (dataConWorkId)


import Unsafe.Coerce

import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative hiding (empty)

import Control.Exception

import Foreign.Storable.Generic.Plugin.Internal.Helpers
import Foreign.Storable.Generic.Plugin.Internal.Error
import Foreign.Storable.Generic.Plugin.Internal.Predicates
import Foreign.Storable.Generic.Plugin.Internal.Types

-- | Compile and expression.
compileExpr :: HscEnv -> CoreExpr -> SrcSpan -> IO a 
compileExpr hsc_env expr src_span = do
    foreign_hval <- liftIO $ hscCompileCoreExpr hsc_env src_span expr
    hval         <- liftIO $ withForeignRef foreign_hval localRef
    let val = unsafeCoerce hval :: a 
    -- finalizeForeignRef foreign_hval  -- check whether that's the source of the error
    return val

-- | Try to compile an expression. Perhaps return an error.
tryCompileExpr :: Id -> CoreExpr -> CoreM (Either Error a)
tryCompileExpr id core_expr  = do
    hsc_env <- getHscEnv
    e_compiled <- liftIO $ try $ 
                    compileExpr hsc_env core_expr (getSrcSpan id) :: CoreM (Either SomeException a)
    case e_compiled of
        Left  se  -> return $ Left $ CompilationError (NonRec id core_expr) (show se)
        Right val-> return $ Right val

----------------------
-- Int substitution --
----------------------

-- | Create an expression of form: \x -> 16
intToExpr :: Type -> Int -> CoreExpr
intToExpr t i = Lam wild $ App fun arg
    where fun = Var $ dataConWorkId intDataCon
          arg = Lit $ MachInt $ fromIntegral i
          wild= mkWildValBinder t 

-- | For gsizeOf and galignment - calculate the variables.
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
            return the_integer >> return $ Left $ CompilationError e "No type found"

-----------------------
-- peek substitution --
-----------------------

-- | Try to substitute the offsets.
offsetSubstitution :: CoreBind -> CoreM (Either Error CoreBind)
offsetSubstitution b@(Rec _) = return $ Left $ CompilationNotSupported b
offsetSubstitution b@(NonRec id expr) = do
    putMsgS "Before"
    putMsg $ ppr expr
    e_subs <- offsetSubstitutionTree [] expr
    let ne_subs = case e_subs of
             Left (OtherError str) -> Left $ CompilationError b str
             a                     -> a
    case e_subs of
        Right e -> putMsgS "After" >> putMsg (ppr e)
        _       -> return ()
    return $ NonRec id <$> e_subs


-- | The data which can be expressed.
data OffsetScope = IntList Id CoreExpr
                 | IntPrimVal  Id CoreExpr

instance Outputable OffsetScope where
    ppr (IntList    id expr) = ppr id <+> ppr expr
    ppr (IntPrimVal id expr) = ppr id <+> ppr expr
    pprPrec _ el = ppr el

-- | Create a list expression from Haskell list.
intListExpr :: [Int] -> CoreExpr
intListExpr list = intListExpr' (reverse list) empty_list 
    where empty_list = App ( Var $ dataConWorkId nilDataCon) (Type intTy)

intListExpr' :: [Int] -> CoreExpr -> CoreExpr
intListExpr'  []    acc = acc
intListExpr' (l:ls) acc = intListExpr' ls $ App int_cons acc
    where int_t_cons = App (Var $ dataConWorkId consDataCon) (Type intTy) 
          int_val    = App (Var $ dataConWorkId intDataCon ) (Lit $ MachInt $ fromIntegral l) 
          int_cons   = App int_t_cons int_val

-- | Compile expression to list and then write it back to core expr.
exprToIntList :: Id -> CoreExpr -> CoreM (Either Error OffsetScope)
exprToIntList id core_expr = do
    int_list <- tryCompileExpr id core_expr
    let new_expr = intListExpr <$> int_list
    return $ IntList id <$> new_expr

-- | Create a int prim expression.
intPrimValExpr :: Int -> CoreExpr
intPrimValExpr i = Lit $ MachInt $ fromIntegral i 

-- | Compile expression to int prim and then write it back to core expr.
exprToIntVal :: Id -> CoreExpr -> CoreM (Either Error OffsetScope)
exprToIntVal id core_expr = do
    int_val <- tryCompileExpr id core_expr
    let new_expr = intPrimValExpr <$> int_val
    return $ IntPrimVal id <$> new_expr

-- | Return the expression if it's a literal or global.
isLitOrGlobal :: CoreExpr -> Maybe CoreExpr
-- Whether it is a literal.
isLitOrGlobal e@(Lit _) = Just e
-- Whether it is a global id:
isLitOrGlobal e@(Var id)
    | isGlobalId id
    = Just e
isLitOrGlobal _ = Nothing

-- | Check whether the expression is compileable.
-- If not, substitute with one in scope.
inScopeVal :: [OffsetScope] -> CoreExpr -> Maybe CoreExpr
-- PrimVal in scope. Check whether this is it.
inScopeVal ((IntPrimVal id n_e):rest) e@(Var v_id)
    | id == v_id
    = Just n_e 
    | otherwise = inScopeVal rest e
-- Something else in scope. Jump to next.
inScopeVal (el:rest)                e@(Var v_id) 
    = inScopeVal rest e
-- Empty list or not Var id.
inScopeVal _ _  = Nothing

-- | Check whether the int list is in scope.
inScopeList :: [OffsetScope] -> CoreExpr -> Maybe CoreExpr
-- TODO: Check whether the expression is a resolveable list.
-- (That is literals and globals, and perhaps values from scope)
-- IntList in scope. Check whether this is it.
inScopeList ((IntList id n_e):rest) e@(Var l_id) 
    | id == l_id
    = Just n_e
    | otherwise = inScopeList rest e
-- Something else in scope. Jump to next.
inScopeList (el:rest) e@(Var l_id) 
    = inScopeList rest e
-- Empty list or not Var id.
inScopeList _ _ = Nothing
-- | Whether the expression is in scope.
inScope :: [OffsetScope] -> CoreExpr -> Maybe CoreExpr
inScope scope expr = inScopeVal scope expr <|> inScopeList scope expr
-- Implement:
-- inScopeVal
-- inScopeList

isIndexer :: Id -> Bool 
isIndexer id = getOccName (varName id) == mkOccName N.varName "$w!!"

-- | Try to create a compileable version of case expr body.
-- For !! @Int offsets val expressions.
caseExprIndex :: [OffsetScope] -> CoreExpr -> Maybe CoreExpr
caseExprIndex scope expr
    -- A long list of what needs to be inside the expression. 
    | App beg lit <- expr
    , Just lit_expr <- inScopeVal scope lit <|> isLitOrGlobal lit
    , App beg2 offsets <- trace "Got lit_expr" $ beg
    , Just list_expr <- trace (showSDocUnsafe $ ppr scope <+> exprInspect offsets) $ inScopeList scope offsets
    , App ix_var t_int <- trace "Got list expr!" beg2
    , Var ix_id    <- ix_var
    , Type intt <- t_int
    , isIntType intt
    , isIndexer ix_id 
    = Just $ App (App (App ix_var t_int) list_expr) lit_expr 
    | otherwise = Nothing

-- | Inspect an expression. Temporary programming scaffolding.
-- For learning purposes.
exprInspect :: CoreExpr -> SDoc
exprInspect (Var id)      =    text "Is id!" <+> ppr id
exprInspect (Lit l)       =    text "is literal" <+> ppr l
exprInspect (App beg lit) =    text "Application:" 
                            $$ nest 4 (text "left"  <+> (exprInspect beg)) 
                            $$ nest 4 (text "right" <+> (exprInspect lit)) 
exprInspect (Lam id expr) =    text "Lambda:" <+> ppr id 
                            $$ nest 2 (exprInspect expr)
exprInspect (Let b     i) =    text "Let " <+> text "..mumble.."
                            $$ exprInspect i
exprInspect (Case expr cb t alts) 
                          =    text "Case " <+> exprInspect expr <+> text "of"
exprInspect  _            = empty

-- | Substitute the offsets in a tree.
-- All top-level local ids should be alread in place.
-- Now try to compile selected expressions (see note [Selected expressions])
offsetSubstitutionTree :: [OffsetScope] -> CoreExpr -> CoreM (Either Error CoreExpr)
-- Variable. Return it, or try to replace it ? Try to replace... 
offsetSubstitutionTree scope e@(Var  _  )    = do
    let m_subs = inScope scope e
    -- Assuming that the first Just will be chosen.
        new_e = m_subs <|> Just e
    case m_subs of
        Nothing -> return ()
        Just expr -> do 
            putMsg $ text "Found sub!" <+> ppr expr
            putMsg $ text "scope" <+> ppr scope
    case new_e of
        Just e -> return $ Right e
        Nothing -> return $ Left $ OtherError "No alternative to itself for an expression"
-- Literal. Return it.
offsetSubstitutionTree scope e@(Lit  _  )    = return $ Right e
-- Do substitutions for both left and right side of an application.
offsetSubstitutionTree scope e@(App  e1  e2) = do
    subs1 <- offsetSubstitutionTree scope e1
    subs2 <- offsetSubstitutionTree scope e2
    return $ App <$> subs1 <*> subs2
offsetSubstitutionTree scope e@(Cast expr c) = do
    subs <- offsetSubstitutionTree scope expr
    return $ Cast <$> subs <*> pure c
offsetSubstitutionTree scope e@(Tick t expr) = do
    subs <- offsetSubstitutionTree scope expr
    return $ Tick t <$> subs
offsetSubstitutionTree scope e@(Type _  )    = return $ Right e
offsetSubstitutionTree scope e@(Lam  b expr) = do
    subs <- offsetSubstitutionTree scope expr
    return $ Lam b <$> subs 
offsetSubstitutionTree scope expr
    -- Parse let offsets = ... in ... expressions.
    -- Compile offsets and put it in scope for further substitution.
    | Let offset_bind case_expr <- expr
    , NonRec offset_id offset_expr <- offset_bind
    , isOffsetsId offset_id
    = do 
      e_new_s <- exprToIntList offset_id offset_expr
      putMsgS "Got offsets!"
      case e_new_s of
          Left err       -> return $ Left err
          Right int_list -> putMsg (ppr int_list) >> offsetSubstitutionTree (int_list:scope) case_expr
    | Let bind in_expr <- expr
    = do 
      putMsgS "Let binding.."
      subs <- offsetSubstitutionTree scope in_expr
      return $ Let bind <$> subs
    -- Parse case expr of _ I# x# -> ... expressions.
    -- Compile case_expr and put it in scope as x#
    -- case_expr is of format !! @Int offsets 0X
    | Case case_expr _ _ [alt0] <- expr
    , (DataAlt i_prim_con, [x_id], alt_expr) <- alt0
    , i_prim_con == intDataCon
    -- this expression _should_ be compileable.
    , Just new_case_expr <- caseExprIndex scope case_expr
    = do 
      putMsgS "Whee"
      e_new_s <- exprToIntVal x_id new_case_expr 
      case e_new_s of
          Left err       -> return $ Left err
          Right int_val  -> putMsg (ppr int_val) >> offsetSubstitutionTree (int_val:scope) alt_expr
    -- 
    | Case case_expr cb t alts <- expr
    = do
        putMsg $ text "casey casey" <+> ppr case_expr
        e_new_alts <- mapM (\(a, args, a_expr) -> (,,) a args <$> offsetSubstitutionTree scope a_expr) alts
        new_case_expr <- offsetSubstitutionTree scope case_expr
        let err = find (\(_,_,e) -> isLeft e) e_new_alts
        case err of
            Nothing -> return $ Case <$> new_case_expr 
                <*> pure cb <*> pure t <*> pure [(a,b,ne) | (a,b,Right ne)  <- e_new_alts]
            Just (_,_,err) -> return err
            Just _  -> return $ Left $ OtherError "errroror"
    | otherwise = return $ Left $ OtherError $ "bye " ++ showSDocUnsafe (ppr expr)
-----------------
-- compilation --
-----------------



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
        -- Perhaps lint compilex expressions ?
        compiled  = rights e_compiled 
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
           (CompilationNotSupported bind) -> Just bind
           (CompilationError        bind _) -> Just bind
           otherwise                      -> Nothing
   printer errors
   when to_crash $ crasher errors
   return $ mapMaybe ungroup errors
