module Foreign.Storable.Generic.Plugin where

import GhcPlugins

import Data.Maybe

import Foreign.Storable.Generic.Plugin.Internal
import Data.IORef
import Data.List
import Control.Monad (when)

import Foreign.Storable.Generic.Plugin.Internal.Error

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

flags = Flags Some False

orderingPass :: Flags -> IORef [[Type]] -> CoreToDo
orderingPass flags io_ref = CoreDoPluginPass "GStorable - type ordering" 
                                (groupTypes flags io_ref)

substitutionPass :: Flags -> IORef [[Type]] -> CoreToDo
substitutionPass flags io_ref = CoreDoPluginPass "GStorable - substitution" 
                                (gstorableSubstitution flags io_ref)
-- | Checks whether it is a simplifier phase 0
isPhase0 :: CoreToDo 
         -> Bool
isPhase0 (CoreDoSimplify iters simpl_mode) = case sm_phase $ simpl_mode of
    Phase 0 -> True
    _       -> False 
isPhase0 _ = False

afterPhase0 :: [CoreToDo] -> Maybe Int
afterPhase0 todos = findIndex isPhase0 todos 



putPasses :: [CommandLineOption] -> [CoreToDo] -> Int -> CoreM [CoreToDo] 
putPasses opts todos ix = do
    the_ioref <- liftIO $ newIORef []
    let (before,after)   = splitAt ix todos
        ordering   = orderingPass     flags the_ioref
        substitute = substitutionPass flags the_ioref
        -- inlinable  = inlinablePass flags the_ioref
        -- todos = concat [before, [ordering, printing], after', [substitute], after'']
        new_todos = concat [[ordering], before, [substitute] , after]
    return new_todos

-- | Inform about installation errors.
install_err :: Flags -> CoreM ()
install_err flags = do
    let (Flags verb to_crash) = flags
        printer = case verb of
            None -> empty
            Some ->    text "The GStorable plugin does not work without simplifier phases."
                    $$ text "Try to compile the code with -O1 or -O2 optimisation flags."
    when to_crash $ (return $ error "Crashing...")

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
    case afterPhase0 todos of
        Just ix -> putPasses opts todos ix 
        Nothing -> install_err flags >> return todos
