{-# LANGUAGE PatternGuards #-}
module Foreign.Storable.Generic.Plugin.Internal where

-- Management of Core.
import CoreSyn (Bind(..),Expr(..), CoreExpr, CoreBind, CoreProgram, Alt)
import Literal (Literal(..))
import Id  (isLocalId, isGlobalId,Id)
import Var (Var(..))
import Name (getOccName,mkOccName)
import OccName (OccName(..), occNameString)
import qualified Name as N (varName)
import SrcLoc (noSrcSpan)
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
import CoreMonad (putMsg, putMsgS, CoreM)


import TyCon (tyConKind)

import Data.List
import Data.Maybe
import Data.Either
import Data.IORef
import Debug.Trace
import Control.Monad.IO.Class
import Control.Monad 

import Foreign.Storable.Generic.Plugin.Internal.Error
import Foreign.Storable.Generic.Plugin.Internal.Compile
import Foreign.Storable.Generic.Plugin.Internal.GroupTypes
import Foreign.Storable.Generic.Plugin.Internal.Helpers
import Foreign.Storable.Generic.Plugin.Internal.Predicates
import Foreign.Storable.Generic.Plugin.Internal.Types


-- | Put that in a separate module ?
groupTypes_errors :: Flags -> [Error] -> CoreM ()
groupTypes_errors flags errors = do
    let (Flags verb to_crash) = flags
        crasher errs = case errs of
            [] -> return ()
            _  -> error "Crashing..."
        print_header txt = case verb of
            None  -> empty
            other ->    text "Errors while grouping types - types not found for: "
                     $$ nest 5 txt
        print_tyNotF verb id = case verb of
            None  -> empty
            other -> ppr id $$ nest 13 (text "::") <+> ppr (varType id)
        print_err    err = case err of
            TypeNotFound id -> print_tyNotF verb id
            other           -> pprError verb other
        printer errs = case errs of
            [] -> return ()
            ls ->  putMsg $ print_header (vcat (map print_err errs)) 
    -- Do printing
    -- Eventually crash.
    printer errors
    when to_crash $ crasher errors


groupTypes :: Flags -> IORef [[Type]] -> ModGuts -> CoreM ModGuts
groupTypes flags type_order_ref guts = do
    let binds = mg_binds guts
        -- Get GStorable ids.
        all_ids = concatMap getIdsBind binds
        gstorable_ids = filter isAnyGStorable all_ids
        -- Now process them - different ids
        -- will have different type signatures.
        -- It is possible to fetch the types from them.
        m_gstorable_types = map (getGStorableType.varType) gstorable_ids
        -- Grab any errors related to types not found.
        weird_types_zip id m_t = case m_t of
            Nothing -> Just $ TypeNotFound id
            Just _  -> Nothing
        weird_types     =    catMaybes $ zipWith weird_types_zip gstorable_ids m_gstorable_types 
        -- type-list is used instead of type_set because Type has no uniquable instance.
        type_list = [ t | Just t <- m_gstorable_types]
        -- Calculate the type ordering.
        (type_order,m_error) = calcGroupOrder type_list
        -- TODO: Do something with failed ordering.

    groupTypes_errors flags weird_types
    
    liftIO $ writeIORef type_order_ref type_order
    return guts



grouping_errors :: Flags -> Maybe Error -> CoreM [CoreBind]
grouping_errors flags m_err = do
   let (Flags _ to_crash) = flags
       verb = Some
       crasher m_e = case m_e of
           Nothing -> return ()
           Just _  -> error "Crashing..."
       print_header txt = case verb of
           None  -> empty
           other ->    text "Errors while grouping bindings: "
                    $$ nest 5 txt 
       printer m_err = case m_err of
           Nothing  -> return ()
           Just err ->  putMsg $ print_header (pprError verb err) 
       ungroup m_e = case m_e of
           Just (OrderingFailedBinds _ rest) -> rest
           _                                 -> []
   printer m_err
   when to_crash $ crasher m_err
   return $ ungroup m_err


isWhat :: Type -> String
isWhat (AppTy t1 _) = "app ty " ++ showSDocUnsafe (ppr t1)
isWhat (TyVarTy t ) = "tyvarty " ++ showSDocUnsafe (ppr t)
isWhat (TyConApp t ts ) = "tyconapp " ++ showSDocUnsafe (cat [ppr t, ppr ts] )
isWhat (ForAllTy tb t ) = "forallTy " ++ showSDocUnsafe (cat$ [ppr tb,ppr t])
isWhat (LitTy t ) = "LitTy " ++ showSDocUnsafe (ppr t)
isWhat otherwise  = "bla"

-- Do something to distinguish fully parametrised types from non-parametrised ones.
gstorableSubstitution :: Flags -> IORef [[Type]] -> ModGuts -> CoreM ModGuts
gstorableSubstitution flags type_order_ref guts = do 
    type_hierarchy <- liftIO $ readIORef type_order_ref 
    let binds  = mg_binds guts
        -- Doing it in one pass.
        -- predicate = toIsBind (withTypeCheck getGStorableType isGStorableMethodId)
        predicate = toIsBind (withTypeCheck getGStorableMethodType isGStorableMethodId)
        -- predicate = toIsBind isGStorableMethodId
        (gstorable_binds,rest) = partition predicate binds
        -- Check if there are any recursives somehow
        -- The plugin won't be able to handle them.
        (nonrecs, recs) = partition isNonRecBind gstorable_binds
        -- Group the gstorables by nestedness
        (grouped_binds, m_err_group) = groupBinds type_hierarchy nonrecs
    

    -- Check for errors
    not_grouped <- grouping_errors flags m_err_group
    -- Compile and replace gstorable bindings
    new_gstorables <- compileGroups flags grouped_binds rest -- perhaps return errors here ?
    
    return $ guts {mg_binds = concat [new_gstorables, not_grouped,recs,rest]}
