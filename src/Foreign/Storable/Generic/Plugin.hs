module Foreign.Storable.Generic.Plugin where

import GhcPlugins hiding (isRec)
import Foreign.Storable.Generic.Plugin.Internal

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

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

simpl1 = CoreDoSimplify 3 mode
    where mode = SimplMode ["whee"] (Phase 10) True True False False

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todo = do
    putMsgS "Yaay"
    let (before,after)  = splitAt 0 todo
        (after1,after2) = splitAt 0 after
    let pass_basic = CoreDoPluginPass "GStorable: evaluate gsizeOf and galignment" basicMethodsPass
    -- return $ todo++[pass_basic]
    putMsg $ ppr before
    let todos = [before, [simpl1, pass_basic], after1, [printCorePass],after2]
    return $ concat todos -- todo++[pass_basic]
