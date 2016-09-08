module Foreign.Storable.Generic.Plugin.Internal.Compile where

-- Management of Core.
import CoreSyn (Bind(..),Expr(..), CoreExpr, CoreBind, CoreProgram, Alt, AltCon(..))
import Literal (Literal(..))
import Id  (isLocalId, isGlobalId,Id)
import Var (Var(..))
import Name (getOccName,mkOccName, getSrcSpan)
import OccName (OccName(..), occNameString)
import qualified Name as N (varName, tvName, tcClsName)
import SrcLoc (noSrcSpan, SrcSpan)
import Unique (getUnique)
-- import PrelNames (intDataConKey)
-- import FastString (mkFastString)
-- import TysPrim (intPrimTy)
-- Compilation pipeline stuff
import HscMain (hscCompileCoreExpr)
import HscTypes (HscEnv,ModGuts(..))
import CoreMonad (CoreM, SimplifierMode(..),CoreToDo(..), getHscEnv, getDynFlags)
import CoreLint (lintExpr)
import BasicTypes (CompilerPhase(..))
-- Haskell types 
import Type (isAlgType, splitTyConApp_maybe)
import TyCon (tyConName, algTyConRhs, visibleDataCons)
import TyCoRep (Type(..), TyBinder(..))
import TysWiredIn (intDataCon)
import DataCon    (dataConWorkId,dataConOrigArgTys) 

import MkCore (mkWildValBinder)
-- Printing
import Outputable (cat, ppr, SDoc, showSDocUnsafe)
import Outputable (Outputable(..),($$), ($+$), vcat, empty,text, (<>), (<+>), nest, int, comma) 
import CoreMonad (putMsg, putMsgS)

-- Used to get to compiled values
import GHCi.RemoteTypes

-- Used to create types
import TysWiredIn
import PrelNames (buildIdKey, augmentIdKey)
import DataCon (dataConWorkId)
import BasicTypes (Boxity(..))

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

---------------------
-- compile helpers --
---------------------

-- | Compile an expression.
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
        Left  se  -> return $ Left $ CompilationError (NonRec id core_expr) (stringToPpr $ show se)
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
intSubstitution b@(Rec    _) = return $ Left $ CompilationNotSupported b
intSubstitution b@(NonRec id (Lam _ (Lam _ _))) = return $ Left $ CompilationNotSupported b 
intSubstitution b@(NonRec id (Lam _ expr)) = do
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
            return the_integer >> return $ Left $ CompilationError b (text "Type not found")

-----------------------
-- peek substitution --
-----------------------

-- | Try to substitute the offsets.
offsetSubstitution :: CoreBind -> CoreM (Either Error CoreBind)
offsetSubstitution b@(Rec _) = return $ Left $ CompilationNotSupported b
offsetSubstitution b@(NonRec id expr) = do
    e_subs <- offsetSubstitutionTree [] expr
    
    let ne_subs = case e_subs of
             -- Add the text from other error.
             Left (OtherError sdoc) 
                 -> Left $ CompilationError b sdoc
             -- Add the information about uncompiled expr.
             Left err@(CompilationError _ _) 
                 -> Left $ CompilationError b (pprError Some err)
             a   -> a
    
    return $ NonRec id <$> e_subs


-- | Scoped variables for optimising offsets.
data OffsetScope = IntList Id CoreExpr
                 | IntPrimVal  Id CoreExpr

getScopeId   :: OffsetScope -> Id
getScopeId (IntList      id _) = id
getScopeId (IntPrimVal   id _) = id

getScopeExpr :: OffsetScope -> CoreExpr
getScopeExpr (IntList      _ expr) = expr 
getScopeExpr (IntPrimVal   _ expr) = expr 

instance Outputable OffsetScope where
    ppr (IntList    id expr) = ppr id <+> ppr (getUnique id) <+> comma <+> ppr expr
    ppr (IntPrimVal id expr) = ppr id <+> ppr (getUnique id) <+> comma <+> ppr expr
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

-- | Check whether the given CoreExpr is an id, 
-- and if yes - substitute it.
inScopeAll :: [OffsetScope] -> CoreExpr -> Maybe CoreExpr
inScopeAll (el:rest) e@(Var v_id) 
    | id <- getScopeId el
    -- Thought uniques will be unique inside.
    , id == v_id
    -- Check whether the types have the same name and id.
    , getOccName (varName id) == getOccName (varName v_id)
    = Just $ getScopeExpr el
    | otherwise = inScopeAll rest e
inScopeAll _  _ = Nothing


-- | Is an "$w!!" identifier
isIndexer :: Id   
          -> Bool
isIndexer id = getOccName (varName id) == mkOccName N.varName "$w!!"

-- | Try to create a compileable version of case expr body.
-- For !! @Int offsets val expressions.
caseExprIndex :: [OffsetScope] -> CoreExpr -> Maybe CoreExpr
caseExprIndex scope expr
    -- A long list of what needs to be inside the expression. 
    | App beg lit <- expr
    -- Substitute or leave the literal be. Otherwise cancel.
    , Just lit_expr <- inScopeAll scope lit <|> isLitOrGlobal lit
    , App beg2 offsets <- beg
    -- Substitute or leave the offsets list free.
    , Just list_expr <- inScopeAll scope offsets <|> Just offsets
    , App ix_var t_int <- beg2
    -- Get to the !! var.
    , Var ix_id    <- ix_var
    -- Check whether types are ok.
    , Type intt <- t_int
    , isIntType intt
    , isIndexer ix_id
    -- New expression.
    = Just $ App (App (App ix_var t_int) list_expr) lit_expr 
    | otherwise = Nothing


{- Note [Offset substitution]
 - ~~~~~~~~~~~~~~~~~~~~~~~~~~
 -
 - We would like for gpeekByteOff and gpokeByteOff methods to work as fast as 
 - handwritten versions. This depends on whether the field's offsets are known
 - at compile time or not. 
 -
 - To have offsets at compile time we have look for certain expressions to pop up.
 - We need to compile them, and later translate them back to Core expressions.
 - This approach relies on compiler optimisations of GStorable internals,
 - like inlining gpeekByteOff' methods and not inlining the calcOffsets functions. 
 - If these optimisations do not happen, a compilation error might occur.
 - If not, the resulting method might be not as fast as handwritten one. 
 -
 -
 - We expect to deal with the following expressions:
 -
 - 
 - 1) let offsets = ... :: [Int] in expr
 -
 - Here we compile the offsets and put them for later use in expr.
 -
 -
 - 2) case $w!! @Int offsets 0# of _ I# x -> alt_expr
 - or case $w!! @Int ...     0# of _ I# x -> alt_expr   
 - 
 - Here we substitute the offsets if we can, and then we compile the 
 - evaluated expression to later replace 'x' occurences in alt_expr.
 -
 -
 -}

-- | Substitute the offsets in a tree.
-- All top-level local ids should be alread in place.
-- Now try to compile selected expressions (See note [Offset substitution])
offsetSubstitutionTree :: [OffsetScope] -> CoreExpr -> CoreM (Either Error CoreExpr)
-- Literal. Return it.
offsetSubstitutionTree scope e@(Lit  _  )    = return $ Right e
-- Do substitutions for both left and right side of an application.
offsetSubstitutionTree scope e@(App  e1  e2) = do
    subs1 <- offsetSubstitutionTree scope e1
    subs2 <- offsetSubstitutionTree scope e2
    return $ App <$> subs1 <*> subs2
-- Do substitution for the expressions in Cast
offsetSubstitutionTree scope e@(Cast expr c) = do
    subs <- offsetSubstitutionTree scope expr
    return $ Cast <$> subs <*> pure c
-- Do substitution for the expressions in Tick
offsetSubstitutionTree scope e@(Tick t expr) = do
    subs <- offsetSubstitutionTree scope expr
    return $ Tick t <$> subs
-- Leave types alone.
offsetSubstitutionTree scope e@(Type _  )    = return $ Right e
-- Do substitutions for the lambda body.
offsetSubstitutionTree scope e@(Lam  b expr) = do
    subs <- offsetSubstitutionTree scope expr
    return $ Lam b <$> subs
-- Other substitutions: For Case, Let, and Var.
offsetSubstitutionTree scope expr
    -- Parse let offsets = ... in ... expressions.
    -- Compile offsets and put it in scope for further substitution.
    | Let    offset_bind in_expr     <- expr
    , NonRec offset_id   offset_expr <- offset_bind
    , isOffsetsId offset_id
    = do 
      e_new_s <- exprToIntList offset_id offset_expr
      case e_new_s of
          Left err       -> return $ Left err
          Right int_list -> offsetSubstitutionTree (int_list:scope) in_expr
    -- Normal let bindings 
    | Let bind in_expr <- expr
    = do 
      subs <- offsetSubstitutionTree scope in_expr
      -- Substitution for the bindings
      let sub_idexpr (id,e) = do
              inner_subs <- offsetSubstitutionTree scope e
              return $ (,) id <$> inner_subs
          sub_bind (NonRec id e) = do
              inner_subs <- offsetSubstitutionTree scope e
              return $ NonRec id <$> inner_subs 
          sub_bind (Rec bs) = do
              inner_subs <- mapM sub_idexpr bs
              case lefts inner_subs of
                  []      -> return $ Right $ Rec (rights inner_subs)
                  (err:_) -> return $ Left err
      bind_subs <- sub_bind bind
      --
      return $ Let <$> bind_subs <*> subs
    -- Parse case expr of _ I# x# -> ... expressions.
    -- Compile case_expr and put it in scope as x#
    -- case_expr is of format $w!! @Int offsets 0#
    | Case case_expr _ _ [alt0] <- expr
    , (DataAlt i_prim_con, [x_id], alt_expr) <- alt0
    , i_prim_con == intDataCon
    , Just new_case_expr <- caseExprIndex scope case_expr
    = do 
      e_new_s <- exprToIntVal x_id new_case_expr 
      case e_new_s of
          Left err       -> return $ Left err
          Right int_val  ->  offsetSubstitutionTree (int_val:scope) alt_expr 
    -- Normal case expressions. 
    | Case case_expr cb t alts <- expr
    = do
        e_new_alts <- mapM (\(a, args, a_expr) -> (,,) a args <$> offsetSubstitutionTree scope a_expr) alts
        new_case_expr <- offsetSubstitutionTree scope case_expr
        -- Find the first error in alternative compilation
        let c_err = find (\(_,_,e) -> isLeft e) e_new_alts
        case c_err of
            Nothing -> return $ Case <$> new_case_expr 
                <*> pure cb <*> pure t <*> pure [(a,b,ne) | (a,b,Right ne)  <- e_new_alts]
            Just (_,_,err) -> return err
    -- Variable. Return it or try to replace it.
    -- Must be here, otherwise other substitutions won't happen
    -- due to replacement of offsets to lists.
    | Var id <- expr
    = do
      let m_subs = inScopeAll scope expr
          new_e = m_subs <|> Just expr
      case new_e of
          Just e -> return $ Right e
          Nothing -> return $ Left $ OtherError  (text  "This shouldn't happen."
                                      $$ text "`m_subs <|> Just e` cannot be `Nothing`.")
    | otherwise = return $ Left $ OtherError $ (text "Unsupported expression:" $$ ppr expr)

-----------------
-- compilation --
-----------------


-- | Compile the expression in Core Bind and replace it.
compileGStorableBind :: CoreBind -> CoreM (Either Error CoreBind) 
compileGStorableBind core_bind
    -- Substitute gsizeOf
    | (NonRec id expr) <- core_bind
    , isSizeOfId id || isSpecSizeOfId id 
    = intSubstitution core_bind
    -- Substitute galignment
    | (NonRec id expr) <- core_bind
    , isAlignmentId id || isSpecAlignmentId id 
    = intSubstitution core_bind
    -- Substitute offsets in peeks.
    | (NonRec id expr) <- core_bind
    , isPeekId id      || isSpecPeekId id
    = offsetSubstitution core_bind
    -- Substitute offsets in pokes.
    | (NonRec id expr) <- core_bind
    , isPokeId id      || isSpecPokeId id
    = offsetSubstitution core_bind
    -- Everything else - nope.
    | otherwise = return $ Left $ CompilationNotSupported core_bind

-- | Lint a binding
lintBind :: CoreBind -- ^ Core binding to use when returning CompilationError
         -> CoreBind -- ^ Core binding to check
         -> CoreM (Either Error CoreBind) -- ^ Success or failure
lintBind b_old b@(NonRec id expr) = do
    dyn_flags <- getDynFlags
    case lintExpr dyn_flags [] expr of
        Just sdoc -> (return $ Left $ CompilationError b_old sdoc)
        Nothing   -> return $ Right b
lintBind b_old b@(Rec bs) = do
    dyn_flags <- getDynFlags
    let errs = mapMaybe (\(_,expr) -> lintExpr dyn_flags [] expr) bs
    case errs of
        [] -> return $ Right b
        _  -> return $ Left $ CompilationError b_old (vcat errs)

-- | Substitutes the localIds inside the bindings with bodies of provided bindings.
replaceIdsBind :: [CoreBind] -- ^ Replace with - for GStorable bindings
               -> [CoreBind] -- ^ Replace with - for other top-bindings
               -> CoreBind   -- ^ Binding which will have ids replaced.
               -> CoreBind   -- ^ Binding with replaced ids.
replaceIdsBind gstorable_bs other_bs (NonRec id e) = NonRec id (replaceIds gstorable_bs other_bs e)
replaceIdsBind gstorable_bs other_bs (Rec    recs) = Rec $ map (\(id,e) -> (id,replaceIds gstorable_bs other_bs e)) recs

-- | Substitutes the localIds inside the expressions with bodies of provided bindings.
replaceIds :: [CoreBind] -- ^ Replace with - for GStorable bindins
           -> [CoreBind] -- ^ Replace with - for other top-bindings
           -> CoreExpr   -- ^ Expression which will have ids replaced.
           -> CoreExpr   -- ^ Expression with replaced ids.
replaceIds gstorable_bs other_bs e@(Var id)
    -- For non recs.
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
-- Replace on the left and right side of application.
replaceIds gstorable_bs other_bs (App e1 e2) = App (replaceIds gstorable_bs other_bs e1) (replaceIds gstorable_bs other_bs e2)
-- Replace the body of lambda expressions.
replaceIds gstorable_bs other_bs (Lam id e)  = Lam id (replaceIds gstorable_bs other_bs e)
-- Replace both bindings and the expressions.
replaceIds gstorable_bs other_bs (Let  b e)  = Let (replaceIdsBind gstorable_bs other_bs b) (replaceIds gstorable_bs other_bs e)
-- Replace the case_expression and the altenatives.
replaceIds gstorable_bs other_bs (Case e ev t alts) = do
    let new_e = replaceIds gstorable_bs other_bs e
        new_alts = map (\(alt, ids, exprs) -> (alt,ids, replaceIds gstorable_bs other_bs exprs)) alts
    Case new_e ev t new_alts
-- Replace the expression in Cast
replaceIds gstorable_bs other_bs (Cast e c) = Cast (replaceIds gstorable_bs other_bs e) c
-- Replace the expression in ticks.
replaceIds gstorable_bs other_bs (Tick t e) = Tick t (replaceIds gstorable_bs other_bs e)
-- For anything else - just return it.
replaceIds gstorable_bs other_bs e          = e

-- | Compile ordered binding.
compileGroups :: Flags            -- ^ Error handling.
              -> [[CoreBind]]     -- ^ Ordered gstorable bindings.
              -> [CoreBind]       -- ^ Non-gstorable bindings, used for replacing ids.
              -> CoreM [CoreBind] -- ^ The compiled (or not) bindings.
compileGroups flags bind_groups bind_rest = compileGroups_rec flags 0 bind_groups bind_rest [] []


-- | The insides of compileGroups method.
compileGroups_rec :: Flags         -- ^ For error handling.
                  -> Int           -- ^ Depth, useful for debugging.
                  -> [[CoreBind]]  -- ^ Ordered GStorable bindings. 
                  -> [CoreBind]    -- ^ Other top-level bindings
                  -> [CoreBind]    -- ^ Succesfull substitutions.
                  -> [CoreBind]    -- ^ Unsuccesfull substitutions.
                  -> CoreM [CoreBind] -- ^ Both successfull and unsuccesfull subtitutions.
compileGroups_rec flags _ []       bind_rest subs not_subs = return $ concat [subs,not_subs]
compileGroups_rec flags d (bg:bgs) bind_rest subs not_subs = do
    let layer_replaced = map (replaceIdsBind bind_rest subs) bg
    -- Compile and then lint.
        compile_and_lint bind = do
            e_compiled <- compileGStorableBind bind
            -- Monad transformers would be nice here.
            case e_compiled of
                Right bind' -> lintBind bind bind'
                _           -> return e_compiled 
    -- Compiled (or not) expressions
    e_compiled <- mapM compile_and_lint layer_replaced
    let errors = lefts e_compiled
        compiled  = rights e_compiled 
    
    -- Handle errors    
    not_compiled <- compileGroups_error flags d errors
    -- Next iteration.
    compileGroups_rec flags (d+1) bgs bind_rest (concat [compiled,subs]) (concat [not_compiled, not_subs])

-- | Handle errors during the compileGroups stage.
compileGroups_error :: Flags            -- ^ Error handling
                    -> Int              -- ^ Current iteration
                    -> [Error]          -- ^ List of errors
                    -> CoreM [CoreBind] -- ^ Bindings from errors.
compileGroups_error flags d errors = do
   let (Flags verb to_crash) = flags
       -- To crash handler
       crasher errs = case errs of
           []   -> return ()
           _    -> error "Crashing..."
       -- Print header for this type of errors
       print_header txt = case verb of
           None  -> empty
           other ->    text "Errors while compiling and substituting bindings at depth " <+> int d <> text ":" 
                    $$ nest 4 txt 
       -- Print errors themselves
       printer errs = case errs of
           [] -> return ()
           -- Print with header
           ls ->  putMsg $ print_header (vcat (map (pprError verb) errs)) 
       -- Get the bindings from errors.
       ungroup err = case err of
           (CompilationNotSupported bind)   -> Just bind
           (CompilationError        bind _) -> Just bind
           -- If we get Nothing, we will probably get missing symbols.
           -- TODO: Handle such situations.
           _                               -> Nothing

   -- Print errors
   printer errors
   -- Crash if conditions are met
   when to_crash $ crasher errors
   -- Return bindings
   return $ mapMaybe ungroup errors
