module Foreign.Storable.Generic.Plugin where

import GhcPlugins hiding (isRec)
import TyCoRep
import TyCon
import DataCon

import Data.Maybe

import qualified Foreign.Storable.Generic.Plugin.SimplCore as SC
import Foreign.Storable.Generic.Plugin.Internal
import Foreign.Storable.Generic.Plugin.Internal.InstanceDecl hiding (getType)



isRec :: CoreBind -> Bool
isRec (Rec _) = True
isRec _       = False

printCorePass :: CoreToDo
printCorePass = CoreDoPluginPass "print" printing
    where printing mod_guts = (putMsg $ ppr (mg_binds mod_guts)) >> return mod_guts

basicMethodsPass :: ModGuts -> CoreM ModGuts
basicMethodsPass mod_guts = do
    let core_prog = mg_binds mod_guts
    hsc_env <- getHscEnv
    -- putMsg $ ppr core_prog
    new_core_prog <- liftIO $ forceCompileTimeEvaluation hsc_env core_prog
    -- putMsgS $ "***\n***\n***\n"
    -- putMsg $ ppr new_core_prog
    putMsgS $ "There are n binds, x recusrive: " ++ show (length core_prog) ++ " " ++ show (length (filter isRec core_prog)) 
    -- putMsg $ ppr (filter isRec core_prog)
    return mod_guts { mg_binds = new_core_prog}
    -- return mod_guts

getType :: Type -> Maybe Type
getType (TyConApp _ [t]) = Just t
getType otherwise   = Nothing

findInstDecls :: CoreToDo
findInstDecls = CoreDoPluginPass "find inst decls" $ \guts -> do
    let binds      = mg_binds guts
        gstorables = filter ((any isGStorableDictId).getIdsBind) binds
        dicts      = filter isGStorableDictId $ concatMap getIdsBind gstorables
        types      = mapMaybe (getType.varType) dicts
        is_alg     = map isAlgType types
        data_cons  = map (visibleDataCons.algTyConRhs.fst) $ mapMaybe splitTyConApp_maybe types
        fields     = map (map dataConUnivTyBinders) data_cons
        fields2     = map (map dataConOrigArgTys) data_cons
    putMsgS "Trying to find GStorable instances here.."
    putMsg $ ppr $ map getIdsBind binds 
    putMsgS "Dictionaries themselves"
    putMsg $ ppr $ dicts
    putMsgS "Types and their contents"
    putMsg $ cat $ map (text.isWhat) types
    putMsgS "Are they algebraic ?"
    putMsg $ ppr is_alg
    putMsgS "Get somehow inside ?"
    putMsg $ ppr $ map (visibleDataCons.algTyConRhs.fst) $ mapMaybe splitTyConApp_maybe types
    putMsg $ ppr fields
    putMsg $ ppr fields2
    return guts

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

simpl1 = CoreDoSimplify 2 mode
    where mode = SimplMode ["Simplifier phase for optimising gsizeOf and galignment methods"] (Phase (-1)) True True True False


mySimpl1 :: CoreToDo
mySimpl1 = CoreDoPluginPass "My own simplifier phase!" simply_simp
    where simply_simp = SC.simplifyPgm $ CoreDoSimplify 2 (SimplMode [] (Phase (-1)) True True True True)

simpl_0 = CoreDoSimplify 1 mode
    where mode = SimplMode ["Simplifier phase for optimising gsizeOf and galignment methods"] (Phase 0) True True True True

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todo = do
    putMsgS "Yaay"
    let (before,after)  = splitAt 5 todo
        (after1,after2) = splitAt 0 after
    let pass_basic = CoreDoPluginPass "GStorable: evaluate gsizeOf and galignment" basicMethodsPass
        pass_decls = CoreDoPluginPass "Blah" actionFun
      -- return $ todo++[pass_basic]
    let todos = [[], before, [printCorePass], after1, [],after2]
    putMsg $ ppr todos
    return $ concat todos -- todo++[pass_basic]
