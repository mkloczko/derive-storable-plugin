module Foreign.Storable.Generic.Plugin.Internal.Error where

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
data Error = TypeNotFound Id     -- ^ Could not obtain the type from the id.
           | RecBinding CoreBind -- ^ The binding is recursive and won't be substituted.
           | CompilationNotSupported CoreBind -- ^ The compilation-substitution is not supported for this.
           | CompilationError        CoreBind String
           | CompilationErrorExpr    CoreExpr
           | OrderingFailedBinds Int [CoreBind]
           | OrderingFailedTypes Int [Type]
           | OtherError          String

-- | How to print type not found
pprTypeNotFound :: Verbosity -> Id -> SDoc
pprTypeNotFound None _  = empty 
pprTypeNotFound Some id 
    =    text "Could not obtain the type from" 
      $$ nest 5 (ppr id <+> text "::" <+> ppr (varType id) )  
pprTypeNotFound All id  = pprTypeNotFound Some id

-- | Printing RecBinding error
pprRecBinding :: Verbosity -> CoreBind -> SDoc
pprRecBinding None _ = empty
pprRecBinding Some (Rec bs) 
    =    text "The binding is recursive and won't be substituted"
      $$ nest 5 (vcat ppr_ids)
    where ppr_ids = map (\(id,_) -> ppr id <+> text "::" <+> ppr (varType id) ) bs
pprRecBinding Some (NonRec id _) 
    =    text "RecBinding error for non recursive binding...?"
      $$ nest 5 (ppr id <+> text "::" <+> ppr (varType id) )  
pprRecBinding All  b@(Rec _) 
    =     text "--- The binding is recursive and won't be substituted ---"
      $+$ ppr b
      $+$ text "------"
pprRecBinding All  b@(NonRec _ _) 
    =     text "--- RecBinding error for non recursibe binding ? ---"
      $+$ ppr b
      $+$ text "------"

-- | Printing CompilationNotSupported error
pprCompilationNotSupported :: Verbosity -> CoreBind -> SDoc
pprCompilationNotSupported None _   = empty
pprCompilationNotSupported Some bind 
    =    text "Compilation is not supported for bindings of the following format: "
      $$ nest 5 (vcat ppr_ids)
    where ppr_ids = map (\id -> ppr id <+> text "::" <+> ppr (varType id) ) $ getIdsBind bind
pprCompilationNotSupported All  bind 
    =     text "--- Compilation is not supported for bindings of the following format ---"
      $+$ ppr bind 
      $+$ text "------"



-- | Printing CompilationError error
pprCompilationError :: Verbosity -> CoreBind -> String -> SDoc
pprCompilationError None _ _  = empty
pprCompilationError Some bind str
    =    text "Compilation failed for the following binding: "
      $$ nest 5 (vcat ppr_ids)
      $$ nest 5 (text "The error was:" $$ nest 5 (text str))
    where ppr_ids = map (\id -> ppr id <+> text "::" <+> ppr (varType id) ) $ getIdsBind bind
pprCompilationError All  bind str
    =     text "--- Compilation failed for the following binding ---"
      $+$ text "Error message: "
      $+$ text str
      $+$ ppr bind 
      $+$ text "------"

-- | Printing OrderingFailedTypes error
pprOrderingFailedTypes :: Verbosity -> Int -> [Type] -> SDoc
pprOrderingFailedTypes None _ _ = empty
pprOrderingFailedTypes Some depth types 
    =    text "Type ordering failed at depth" <+> int depth <+> text "for types:"
      $$ nest 5 (vcat ppr_types)
    where ppr_types = map ppr types
pprOrderingFailedTypes All  depth types = pprOrderingFailedTypes Some depth types

-- | Printing OrderingFailedBinds error
pprOrderingFailedBinds :: Verbosity -> Int -> [CoreBind] -> SDoc
pprOrderingFailedBinds None _ _ = empty
pprOrderingFailedBinds Some depth binds 
    =    text "CoreBind ordering failed at depth" <+> int depth <+> text "for bindings:"
      $$ nest 5 (vcat ppr_ids)
    where ppr_ids = map (\id -> ppr id <+> text "::" <+> ppr (varType id) ) $ concatMap getIdsBind binds
pprOrderingFailedBinds All  depth binds
    =     text "--- CoreBind ordering failed at depth" <+> int depth <+> text "for bindings ---"
      $+$ nest 5 (vcat ppr_binds)
      $+$ text "------"
    where ppr_binds = map ppr binds

pprOtherError :: Verbosity -> String -> SDoc
pprOtherError None _   = empty
pprOtherError _    str = text str

pprError :: Verbosity -> Error -> SDoc
pprError verb (TypeNotFound            id  ) = pprTypeNotFound verb id
pprError verb (RecBinding              bind) = pprRecBinding   verb bind
pprError verb (CompilationNotSupported bind) = pprCompilationNotSupported verb bind
pprError verb (CompilationError    bind str) = pprCompilationError verb bind str
pprError verb (OrderingFailedBinds d    bs) = pprOrderingFailedBinds verb d bs
pprError verb (OrderingFailedTypes d    ts) = pprOrderingFailedTypes verb d ts
pprError verb (OtherError          str    ) = pprOtherError          verb str
