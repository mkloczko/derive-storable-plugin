{-# LANGUAGE PatternGuards #-}
module Foreign.Storable.Generic.Plugin.Internal where

-- Management of Core.
import CoreSyn (Bind(..),Expr(..), CoreExpr, CoreBind, CoreProgram, Alt)
import Literal (Literal(..))
import Id  (isLocalId, isGlobalId,Id, modifyInlinePragma, setInlinePragma, idInfo)
import IdInfo 
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
import CoreMonad (CoreM, SimplifierMode(..),CoreToDo(..), getHscEnv, getDynFlags)
import BasicTypes (CompilerPhase(..))
-- Haskell types 
import Type (isAlgType, splitTyConApp_maybe)
import TyCon (algTyConRhs, visibleDataCons)
import TyCoRep (Type(..), TyBinder(..))
import TysWiredIn (intDataCon)
import DataCon    (dataConWorkId,dataConOrigArgTys) 

import MkCore (mkWildValBinder)
-- Printing
import Outputable (cat, ppr, SDoc, showSDocUnsafe, showSDoc)
import Outputable (($$), ($+$), hsep, vcat, empty,text, (<>), (<+>), nest, int, colon,hcat, comma, punctuate) 
import CoreMonad (putMsg, putMsgS, CoreM)

import TyCon
import DataCon
import TyCon (tyConKind)
import BasicTypes

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



--------------------
-- Grouping types --
--------------------


groupTypes_errors :: Flags -> [Error] -> CoreM ()
groupTypes_errors flags errors = do
    let (Flags verb to_crash) = flags
        crasher errs = case errs of
            [] -> return ()
            _  -> error "Crashing..."
        print_header txt = case verb of
            None  -> empty
            other ->    text "Errors while grouping types - types not found for: "
                     $$ nest 4 txt
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

groupTypes_info :: Flags -> [[Type]] -> CoreM ()
groupTypes_info flags types = do
    let (Flags verb _) = flags
        -- If verbosity is set, do the printing
        print_header txt = case verb of
            None  -> empty
            other ->    text "GStorable instances will be optimised in the following order"
                    $+$ nest 4 txt
                    $+$ text ""
        print_layer layer ix = int ix <> text ":" <+> hsep (punctuate comma $ map ppr layer)
        -- Print groups of types
        printer groups = case groups of
            [] -> return ()
            _  -> putMsg $ print_header (vcat $ zipWith print_layer groups [1..])
    -- Do the printing
    printer types

groupTypes :: Flags -> IORef [[Type]] -> ModGuts -> CoreM ModGuts
groupTypes flags type_order_ref guts = do
    let binds = mg_binds guts
        -- Get GStorable ids that are fully defined.
        all_ids = concatMap getIdsBind binds
        predicate id = and [ isGStorableId id
                           , not (hasGStorableConstraints $ varType id)
                           ]
        gstorable_ids = filter predicate all_ids
        -- Now process them - different ids
        -- will have different type signatures.
        -- It is possible to fetch the types from them.
        m_gstorable_types = map (getGStorableType.varType) gstorable_ids
        -- Grab any errors related to types not found.
        bad_types_zip id m_t = case m_t of
            Nothing -> Just $ TypeNotFound id
            Just _  -> Nothing
        bad_types     =    catMaybes $ zipWith bad_types_zip gstorable_ids m_gstorable_types 
        -- type_list is used instead of type_set because Type has no uniquable instance.
        type_list = [ t | Just t <- m_gstorable_types]
        -- Calculate the type ordering.
        (type_order,m_error) = calcGroupOrder type_list
    
    groupTypes_info flags type_order
    groupTypes_errors flags bad_types
    
    liftIO $ writeIORef type_order_ref type_order
    return guts


modifyIds :: (Id -> Id) -> CoreBind -> CoreBind
modifyIds f (NonRec id expr) = NonRec (f id) expr
modifyIds f (Rec bs) = Rec $ map (\(id,expr) -> (f id, expr) ) bs

-- Wonder whether a frontend plugin wouldn't do it's job here.
changeInlining :: Flags -> IORef [[Type]] -> ModGuts -> CoreM ModGuts
changeInlining flags type_order_ref guts = do 
    type_hierarchy <- liftIO $ readIORef type_order_ref 
    let binds  = mg_binds guts
        -- Get all GStorable binds.
        -- Check whether the type has GStorable constraints.
        typeCheck t = if hasGStorableConstraints t
            then getGStorableMethodType t
            else Nothing
        predicate = toIsBind (withTypeCheck typeCheck isGStorableMethodId)
        
        (gstorable_binds,rest) = partition predicate binds
        -- Modify Inline Pragmas.
        setInlinable inl_prag = inl_prag {inl_inline = Inline  }
        new_gstorables = map (modifyIds (\id ->  modifyInlinePragma id (setInlinable)  )) gstorable_binds
    putMsg $ ppr $ concatMap getIdsBind new_gstorables
    putMsg $ ppr $ map (inlinePragInfo.idInfo) $ concatMap getIdsBind gstorable_binds
    putMsg $ ppr $ map (inlinePragInfo.idInfo) $ concatMap getIdsBind new_gstorables
        
    return $ guts {mg_binds = concat [new_gstorables, rest] }

------------------------------------------------
-- Grouping and compiling GStorable CoreBinds --
------------------------------------------------

-- | Print errors related to CoreBind grouping.
-- Return the badly grouped bindings, and perhaps crash
-- the compiler.
grouping_errors :: Flags            -- ^ Verbosity and ToCrash options 
                -> Maybe Error      -- ^ The error
                -> CoreM [CoreBind] -- ^ Recovered bindings.
grouping_errors flags m_err = do
   let (Flags _ to_crash) = flags
       verb = Some
       crasher m_e = case m_e of
           Nothing -> return ()
           Just _  -> error "Crashing..."
       print_header txt = case verb of
           None  -> empty
           other ->    text "Errors while grouping bindings: "
                    $$ nest 4 txt 
       printer m_err = case m_err of
           Nothing  -> return ()
           Just err ->  putMsg $ print_header (pprError verb err) 
       ungroup m_e = case m_e of
           Just (OrderingFailedBinds _ rest) -> rest
           _                                 -> []
   printer m_err
   when to_crash $ crasher m_err
   return $ ungroup m_err


-- | Print the information related to found GStorable ids.
foundBinds_info :: Flags    -- ^ Verbosity and ToCrash options 
                -> [Id]     -- ^ GStorable ids.
                -> CoreM ()
foundBinds_info flags ids = do
    -- For Pretty printing
    dyn_flags <- getDynFlags
    let (Flags verb _) = flags
        -- If verbosity is set, do the printing
        print_header txt = case verb of
            None  -> empty
            other ->    text "The following bindings are to be optimised:"
                    -- $+$ nest 4 (text "")
                    $+$ nest 4 txt
        print_binding id = ppr id
        -- print_binding id = ppr id $$ nest (max_nest+1) (text "::" <+> (ppr $ varType id))
        --     where len_id = length $ showSDoc dyn_flags $ ppr id 
        max_nest = maximum $ 0 : map (length.(showSDoc dyn_flags).ppr) ids
        -- Print groups of types
        printer the_groups = case the_groups of
            [] -> return ()
            _  -> putMsg $ print_header $ vcat (map print_group the_groups)
        -- Use eqType for maybes
        eqType_maybe (Just t1) (Just t2) = t1 `eqType` t2
        eqType_maybe _         _         = False
        -- group and sort the bindings 
        grouped = groupBy (\i1 i2 -> (getGStorableType $ varType i1) `eqType_maybe` (getGStorableType $ varType i2) ) ids
        sorting = sortBy (\i1 i2 -> varName i1 `compare` varName i2)
        sorted  = map sorting grouped
        -- print groups of bindings
        print_group the_group = case the_group of
            [] -> empty
            (h:_) -> case getGStorableType $ varType h of
                Just gtype ->     ppr  gtype
                              $+$ (hsep $ punctuate comma (map print_binding the_group))
                              $+$ text ""
                Nothing    -> ppr "Could not get the type of a binding:" 
                              $+$ nest 4 (ppr h <+> text "::" <+> ppr (varType h))
    -- Print the ids
    printer sorted

-- | Do the optimisation for GStorable bindings.
gstorableSubstitution :: Flags          -- ^ Verbosity and ToCrash options.
                      -> IORef [[Type]] -- ^ Reference to grouped types.
                      -> ModGuts        -- ^ Information about compiled module.
                      -> CoreM ModGuts  -- ^ Information about compiled module, with GStorable optimisations.
gstorableSubstitution flags type_order_ref guts = do 
    type_hierarchy <- liftIO $ readIORef type_order_ref 
    let binds  = mg_binds guts
        -- Get all GStorable binds.
        -- Check whether the type has no constraints.
        typeCheck t = if hasGStorableConstraints t
            then Nothing
            else getGStorableMethodType t
        predicate = toIsBind (withTypeCheck typeCheck isGStorableMethodId)
        
        (gstorable_binds,rest) = partition predicate binds
        -- Check if there are any recursives somehow
        -- The plugin won't be able to handle them.
        (nonrecs, recs) = partition isNonRecBind gstorable_binds
        -- Group the gstorables by nestedness
        (grouped_binds, m_err_group) = groupBinds type_hierarchy nonrecs
    
    foundBinds_info flags $ concatMap getIdsBind $ concat grouped_binds 
    -- Check for errors
    not_grouped <- grouping_errors flags m_err_group
    -- Compile and replace gstorable bindings
    new_gstorables <- compileGroups flags grouped_binds rest -- perhaps return errors here ?
    
    return $ guts {mg_binds = concat [new_gstorables, not_grouped,recs,rest]}
