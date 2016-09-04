module Foreign.Storable.Generic.Plugin where

import GhcPlugins

import Data.Maybe

import Foreign.Storable.Generic.Plugin.Internal
import Data.IORef

import Foreign.Storable.Generic.Plugin.Internal.Error

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

flags = Flags All False

orderingPass :: Flags -> IORef [[Type]] -> CoreToDo
orderingPass flags io_ref = CoreDoPluginPass "GStorable plugin: Calculate Type Ordering" 
                                (groupTypes flags io_ref)

substitutionPass :: Flags -> IORef [[Type]] -> CoreToDo
substitutionPass flags io_ref = CoreDoPluginPass "GStorable plugin: Substitute" 
                                (gstorableSubstitution flags io_ref)

printIORefPass :: Outputable t => IORef t -> CoreToDo
printIORefPass io_ref = CoreDoPluginPass "IORef output" prog
    where prog x = (putMsg =<< ppr <$> liftIO (readIORef io_ref)) >> return x

-- simpl1 = CoreDoSimplify 2 mode
--     where mode = SimplMode ["Simplifier phase for optimising gsizeOf and galignment methods"] (Phase (-1)) True True True False
-- 
-- 
-- mySimpl1 :: CoreToDo
-- mySimpl1 = CoreDoPluginPass "My own simplifier phase!" simply_simp
--     where simply_simp = SC.simplifyPgm $ CoreDoSimplify 2 (SimplMode [] (Phase (-1)) True True True True)
-- 
-- simpl_0 = CoreDoSimplify 1 mode
--     where mode = SimplMode ["Simplifier phase for optimising gsizeOf and galignment methods"] (Phase 0) True True True True


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todo = do
    putMsgS "Yaay"
    the_ioref <- liftIO $ newIORef []
    let (before,after)   = splitAt 0 todo
        (after',after'') = splitAt 4 after
        ordering   = orderingPass     flags the_ioref
        printing   = printIORefPass         the_ioref
        substitute = substitutionPass flags the_ioref
        todos = concat [before, [ordering, printing], after', [substitute], after'']
    putMsg $ ppr todos
    return todos
