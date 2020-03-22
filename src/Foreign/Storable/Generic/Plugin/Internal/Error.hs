{-|
Module      : Foreign.Storable.Generic.Plugin.Internal.Error
Copyright   : (c) Mateusz Kłoczko, 2016
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : GHC-only

Contains the Error datatype and related pretty print functions.  

-}
module Foreign.Storable.Generic.Plugin.Internal.Error 
    ( Verbosity(..)
    , CrashOnWarning(..)
    , Flags(..)
    , Error(..)
    , pprError
    , stringToPpr
    ) where

import Id (Id)
import Var(Var(..))
import CoreSyn (CoreBind(..), Bind(..),CoreExpr(..))
import Type (Type)
import Outputable

import Foreign.Storable.Generic.Plugin.Internal.Helpers

-- | How verbose should the messages be.
data Verbosity = None | Some | All 

-- | Crash when an recoverable error occurs. For testing purposes.
type CrashOnWarning = Bool

-- | Contains user-specified flags.
data Flags = Flags Verbosity CrashOnWarning

-- | All possible errors.
data Error = TypeNotFound Id                       -- ^ Could not obtain the type from the id.
           | RecBinding CoreBind                   -- ^ The binding is recursive and won't be substituted.
           | CompilationNotSupported CoreBind      -- ^ The compilation-substitution is not supported for the given binding.
           | CompilationError        CoreBind SDoc -- ^ Error during compilation. The CoreBind is to be returned.
           | OrderingFailedBinds Int [CoreBind]    -- ^ Ordering failed for core bindings.
           | OrderingFailedTypes Int [Type]        -- ^ Ordering failed for types
           | OtherError          SDoc              -- ^ Any other error.

pprTypeNotFound :: Verbosity -> Id -> SDoc
pprTypeNotFound None _  = empty 
pprTypeNotFound Some id 
    =    text "Could not obtain the type from" 
      $$ nest 4 (ppr id <+> text "::" <+> ppr (varType id) )  
pprTypeNotFound All id  = pprTypeNotFound Some id

pprRecBinding :: Verbosity -> CoreBind -> SDoc
pprRecBinding None _ = empty
pprRecBinding Some (Rec bs) 
    =    text "The binding is recursive and won't be substituted"
      $$ nest 4 (vcat ppr_ids)
    where ppr_ids = map (\(id,_) -> ppr id <+> text "::" <+> ppr (varType id) ) bs
pprRecBinding Some (NonRec id _) 
    =    text "RecBinding error for non recursive binding...?"
      $$ nest 4 (ppr id <+> text "::" <+> ppr (varType id) )  
pprRecBinding All  b@(Rec _) 
    =     text "--- The binding is recursive and won't be substituted ---"
      $+$ text ""
      $+$ nest 4 (ppr b)
      $+$ text ""
pprRecBinding All  b@(NonRec _ _) 
    =     text "--- RecBinding error for non recursive binding ? ---"
      $+$ text ""
      $+$ nest 4 (ppr b)
      $+$ text ""

pprCompilationNotSupported :: Verbosity -> CoreBind -> SDoc
pprCompilationNotSupported None _   = empty
pprCompilationNotSupported Some bind 
    =    text "Compilation is not supported for bindings of the following format: "
      $$ nest 4 (vcat ppr_ids)
    where ppr_ids = map (\id -> ppr id <+> text "::" <+> ppr (varType id) ) $ getIdsBind bind
pprCompilationNotSupported All  bind 
    =     text "--- Compilation is not supported for bindings of the following format ---"
      $+$ text ""
      $+$ nest 4 (ppr bind) 
      $+$ text ""



pprCompilationError :: Verbosity -> CoreBind -> SDoc -> SDoc
pprCompilationError None _ _  = empty
pprCompilationError Some bind sdoc
    =    text "Compilation failed for the following binding: "
      $$ nest 4 (vcat ppr_ids)
      $$ nest 4 (text "The error was:" $$ nest 5 sdoc)
    where ppr_ids = map (\id -> ppr id <+> text "::" <+> ppr (varType id) ) $ getIdsBind bind
pprCompilationError All  bind sdoc
    =     text "--- Compilation failed for the following binding ---"
      $+$ text ""
      $+$ nest 4 (text "Error message: ")
      $+$ nest 4 sdoc
      $+$ text ""
      $+$ nest 4 (ppr bind) 
      $+$ text ""


pprOrderingFailedTypes :: Verbosity -> Int -> [Type] -> SDoc
pprOrderingFailedTypes None _ _ = empty
pprOrderingFailedTypes Some depth types 
    =    text "Type ordering failed at depth" <+> int depth <+> text "for types:"
      $$ nest 4 (vcat ppr_types)
    where ppr_types = map ppr types
pprOrderingFailedTypes All  depth types = pprOrderingFailedTypes Some depth types

pprOrderingFailedBinds :: Verbosity -> Int -> [CoreBind] -> SDoc
pprOrderingFailedBinds None _ _ = empty
pprOrderingFailedBinds Some depth binds 
    =    text "CoreBind ordering failed at depth" <+> int depth <+> text "for bindings:"
      $$ nest 4 (vcat ppr_ids)
    where ppr_ids = map (\id -> ppr id <+> text "::" <+> ppr (varType id)) $ concatMap getIdsBind binds
pprOrderingFailedBinds All  depth binds
    =     text "--- CoreBind ordering failed at depth" <+> int depth <+> text "for bindings ---"
      $+$ text "\n"
      $+$ nest 4 (vcat ppr_binds)
      $+$ text ""
    where ppr_binds = map ppr binds

pprOtherError :: Verbosity -> SDoc -> SDoc
pprOtherError None _   = empty
pprOtherError _    sdoc = sdoc

-- | Print an error according to verbosity flag.
pprError :: Verbosity -> Error -> SDoc
pprError verb (TypeNotFound            id  ) = pprTypeNotFound verb id
pprError verb (RecBinding              bind) = pprRecBinding   verb bind
pprError verb (CompilationNotSupported bind) = pprCompilationNotSupported verb bind
pprError verb (CompilationError    bind str) = pprCompilationError verb bind str
pprError verb (OrderingFailedBinds d    bs) = pprOrderingFailedBinds verb d bs
pprError verb (OrderingFailedTypes d    ts) = pprOrderingFailedTypes verb d ts
pprError verb (OtherError          sdoc   ) = pprOtherError          verb sdoc


-- | Change String to SDoc.
-- Each newline is $$ed with nest equal to spaces before.
-- \t is 4.
stringToPpr :: String -> SDoc
stringToPpr str = do
    -- Whether to take a letter
    let taker   ' ' = True
        taker  '\t' = True
        taker  _    = False
    -- Whether to 
        to_num  ' ' = 1
        to_num '\t' = 4
        to_num _    = 0
    -- Function doing the nesting
    let nest_text str = do
            let whites = takeWhile taker str
                rest   = dropWhile taker str
                num    = sum $ map to_num whites
            nest num $ text rest
    vcat $ map nest_text $ lines str
