{-|
Module      : Foreign.Storable.Generic.Plugin
Copyright   : (c) Mateusz Kłoczko, 2016
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : GHC-only

GHC Core plugin for optimising GStorable instances. 
For more information please refer to generic-storable package.

How to enable:

    * use @-fplugin Foreign.Storable.Generic.Plugin@ option
    * add @\{\-\# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin \#\-\}@ to the compiled module.

-}
{-# LANGUAGE CPP #-}
module Foreign.Storable.Generic.Plugin (plugin) where


#if   MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
import GHC.Plugins
#else
import GhcPlugins
#endif

import Data.Maybe

import Foreign.Storable.Generic.Plugin.Internal
import Data.IORef
import Data.List
import Control.Monad (when)

import Foreign.Storable.Generic.Plugin.Internal.Error

-- | The plugin itself.
plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
#if MIN_VERSION_GLASGOW_HASKELL(8,6,1,0)
  , pluginRecompile = \_ -> pure NoForceRecompile
#endif
  }

defFlags = Flags Some False

orderingPass :: Flags -> IORef [[Type]] -> CoreToDo
orderingPass flags io_ref = CoreDoPluginPass "GStorable - type ordering" 
                                (groupTypes flags io_ref)

substitutionPass :: Flags -> IORef [[Type]] -> CoreToDo
substitutionPass flags io_ref = CoreDoPluginPass "GStorable - substitution" 
                                (gstorableSubstitution flags io_ref)

-- | Checks whether the core pass is a simplifier phase 0.
isPhase0 :: CoreToDo 
         -> Bool
isPhase0 (CoreDoSimplify iters simpl_mode) = case sm_phase $ simpl_mode of
    Phase 0 -> True
    _       -> False 
isPhase0 _ = False

-- | Return the index of simplifier phase 0. 
afterPhase0 :: [CoreToDo] -> Maybe Int
afterPhase0 todos = findIndex isPhase0 todos 

-- | Checks whether the core pass is a specialising pass.
isSpecialize :: CoreToDo -> Bool
isSpecialize CoreDoSpecialising = True
isSpecialize _                  = False

-- | Return the index of the specialising pass. 
afterSpecialize :: [CoreToDo] -> Maybe Int
afterSpecialize todos = findIndex isSpecialize todos 

-- | Set the verbosity and ToCrash flags based on supplied arguments.
setOpts :: Flags -> String -> Flags
setOpts (Flags _    crash) "-v0"    = Flags None crash
setOpts (Flags _    crash) "-v1"    = Flags Some crash
setOpts (Flags _    crash) "-v2"    = Flags All  crash
setOpts (Flags verb _    ) "-crash" = Flags verb True
setOpts flags              opt      = flags

-- | Parse command line options.
parseOpts :: [CommandLineOption] -> Flags
parseOpts opts = foldl' setOpts defFlags opts


putPasses :: Flags -> [CoreToDo] -> Int -> Int -> CoreM [CoreToDo] 
putPasses flags todos ph0 sp = do
    the_ioref <- liftIO $ newIORef []
    let (before_spec,after_spec)   = splitAt sp  todos
        (before_ph0 ,after_ph0)    = splitAt (ph0-sp) after_spec
        ordering   = orderingPass     flags the_ioref
        substitute = substitutionPass flags the_ioref
        new_todos = concat [before_spec, [ordering], before_ph0, [substitute] , after_ph0]
    return new_todos

-- | Inform about installation errors.
install_err :: Flags -> CoreM ()
install_err flags = do
    let (Flags verb to_crash) = flags
        printer = case verb of
            None  -> return ()
            other -> putMsg $ text "The GStorable plugin requires simplifier phases with inlining and rules on, as well as a specialiser phase."
                          $$ text "Try to compile the code with -O1 or -O2 optimisation flags." 
    printer
    when to_crash $ (return $ error "Crashing...")
    


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
    dyn_flags <- getDynFlags
#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
    let opt_level = llvmOptLevel dyn_flags
#else
    let opt_level = optLevel dyn_flags
#endif
        flags     = parseOpts opts
        m_phase0  = afterPhase0     todos
        m_spec    = afterSpecialize todos

    case (m_phase0, m_spec, opt_level) of
        (_       ,_       ,0) -> install_err flags >> return todos
        (Just ph0, Just sp,_) -> putPasses   flags todos (ph0+1) (sp+1)
        (_       ,_       ,_) -> install_err flags >> return todos
