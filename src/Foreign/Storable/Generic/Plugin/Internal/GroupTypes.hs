{-|
Module      : Foreign.Storable.Generic.Plugin.Internal.GroupTypes
Copyright   : (c) Mateusz Kłoczko, 2016
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : GHC-only

Grouping methods, both for types and core bindings.
-}
{-# LANGUAGE CPP #-}
module Foreign.Storable.Generic.Plugin.Internal.GroupTypes 
    (
    -- Type ordering
      calcGroupOrder
    , substituteTyCon
    , getDataConArgs
    -- CoreBind ordering
    , groupBinds
    )
where


#if MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
import GHC.Core          (Bind(..),Expr(..), CoreExpr, CoreBind, CoreProgram, Alt)
import GHC.Types.Literal (Literal(..))
import GHC.Types.Id      (isLocalId, isGlobalId,Id)
import GHC.Types.Var             (Var(..))
import GHC.Types.Name            (getOccName,mkOccName)
import GHC.Types.Name.Occurrence (OccName(..), occNameString)
import qualified GHC.Types.Name as N (varName)
import GHC.Types.SrcLoc (noSrcSpan)
import GHC.Types.Unique (getUnique)
import GHC.Driver.Main (hscCompileCoreExpr, getHscEnv)
import GHC.Driver.Types (HscEnv,ModGuts(..))
import GHC.Core.Opt.Monad (CoreM,CoreToDo(..))
import GHC.Types.Basic (CompilerPhase(..))
import GHC.Core.Type hiding (eqType)
import GHC.Core.TyCon
import GHC.Builtin.Types   (intDataCon)
import GHC.Core.DataCon    (dataConWorkId,dataConOrigArgTys) 
import GHC.Core.Make       (mkWildValBinder)
import GHC.Utils.Outputable (cat, ppr, SDoc, showSDocUnsafe)
import GHC.Utils.Outputable (text, (<+>), ($$), nest)
import GHC.Core.Opt.Monad (putMsg, putMsgS)
#elif MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
import CoreSyn (Bind(..),Expr(..), CoreExpr, CoreBind, CoreProgram, Alt)
import Literal (Literal(..))
import Id  (isLocalId, isGlobalId,Id)
import Var (Var(..))
import Name (getOccName,mkOccName)
import OccName (OccName(..), occNameString)
import qualified Name as N (varName)
import SrcLoc (noSrcSpan)
import Unique (getUnique)
import HscMain (hscCompileCoreExpr)
import HscTypes (HscEnv,ModGuts(..))
import CoreMonad (CoreM,CoreToDo(..), getHscEnv)
import BasicTypes (CompilerPhase(..))
import Type hiding (eqType)
import TyCon 
import TysWiredIn (intDataCon)
import DataCon    (dataConWorkId,dataConOrigArgTys) 
import MkCore (mkWildValBinder)
import Outputable (cat, ppr, SDoc, showSDocUnsafe)
import Outputable (text, (<+>), ($$), nest)
import CoreMonad (putMsg, putMsgS)
#endif



-- Used to get to compiled values
import GHCi.RemoteTypes


#if MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
import GHC.Types.Var (TyVarBinder(..), VarBndr(..))
import GHC.Core.TyCo.Rep (Type(..), TyBinder(..), TyCoBinder(..),scaledThing)
import GHC.Types.Var
#elif MIN_VERSION_GLASGOW_HASKELL(8,8,1,0)
import Var (TyVarBinder(..), VarBndr(..))
import TyCoRep (Type(..), TyBinder(..), TyCoBinder(..))
import Var
#elif MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
import Var (TyVarBndr(..), TyVarBinder)
import TyCoRep (Type(..), TyBinder(..))
import Var
#endif

import Unsafe.Coerce

import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace
import Control.Monad.IO.Class

import Foreign.Storable.Generic.Plugin.Internal.Error
import Foreign.Storable.Generic.Plugin.Internal.Helpers
import Foreign.Storable.Generic.Plugin.Internal.Predicates
import Foreign.Storable.Generic.Plugin.Internal.Types


-- | Calculate the order of types. 
calcGroupOrder :: [Type] -> ([[Type]], Maybe Error)
calcGroupOrder types = calcGroupOrder_rec types []

calcGroupOrder_rec :: [Type]
                   -> [[Type]]
                   -> ([[Type]], Maybe Error)
calcGroupOrder_rec []    acc = (reverse acc, Nothing)
calcGroupOrder_rec types acc = do
    let (layer, rest) = calcGroupOrder_iteration types [] [] []
        layer'       = nubBy eqType layer
    if length layer' == 0
        then (reverse acc, Just $ OrderingFailedTypes (length acc) rest)
        else calcGroupOrder_rec rest (layer':acc)

-- | This could be done more efficently if we'd 
-- represent the problem as a graph problem.
calcGroupOrder_iteration :: [Type] -- ^ Type to check 
                         -> [Type] -- ^ Type that are checked
                         -> [Type] -- ^ Type that are in this layer
                         -> [Type] -- ^ Type that are not.
                         -> ([Type], [Type]) -- Returning types in this layer and the next ones.
calcGroupOrder_iteration []     checked accepted rejected = (accepted, rejected)
calcGroupOrder_iteration (t:ts) checked accepted rejected = do
    let args = getDataConArgs t
        -- Are the t's arguments equal to some other ?
        is_arg_somewhere = any (\t -> elemType t args) checked || any (\t -> elemType t args) ts

    if is_arg_somewhere
        then calcGroupOrder_iteration ts (t:checked)  accepted    (t:rejected)
        else calcGroupOrder_iteration ts (t:checked) (t:accepted)  rejected

-- | Used for type substitution. 
-- Whether a TyVar appears, replace it with a Type.
type TypeScope = (TyVar, Type)

-- | Functions doing the type substitutions.

-- Examples
-- 
-- substituteTyCon [(a,Int)]           a          = Int
-- substituteTyCon [(a,Int),(b,Char)] (AType b a) = AType Char Int
substituteTyCon :: [TypeScope] -> Type -> Type
substituteTyCon []         tc_app             = tc_app
substituteTyCon type_scope old@(TyVarTy  ty_var) 
-- Substitute simple type variables
    = case find (\(av,_) -> av == ty_var) type_scope of
          Just (_, new_type) -> new_type
          Nothing            -> old
substituteTyCon type_scope (TyConApp tc args)
-- Substitute type constructors
    = TyConApp tc $ map (substituteTyCon type_scope) args
substituteTyCon type_scope t = t 

-- | Get data constructor arguments from an algebraic type.
getDataConArgs :: Type -> [Type]
getDataConArgs t 
    | isAlgType t
    , Just (tc, ty_args) <- splitTyConApp_maybe t
    , ty_vars <- tyConTyVars tc
    = do
    -- Substitute data_cons args with type args,
    -- using ty_vars as keys.
    let type_scope = zip ty_vars ty_args
        data_cons  = concatMap dataConOrigArgTys $ (visibleDataCons.algTyConRhs) tc
#if MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
    map (substituteTyCon type_scope) (map scaledThing data_cons)  
#else
    map (substituteTyCon type_scope) data_cons  
#endif
    | otherwise = []



-- | Group bindings according to type groups.
groupBinds :: [[Type]]   -- ^ Type groups.
           -> [CoreBind] -- ^ Should be only NonRecs. 
           -> ([[CoreBind]], Maybe Error)
-- perhaps add some safety so non-recs won't get here.
groupBinds type_groups binds = groupBinds_rec type_groups binds [] 

-- | Iteration for groupBinds
groupBinds_rec :: [[Type]]      -- ^ Group of types
               -> [CoreBind]    -- ^ Ungrouped bindings
               -> [[CoreBind]]  -- ^ Grouped bindings
               -> ([[CoreBind]], Maybe Error) -- ^ Grouped bindings, and perhaps an error)
groupBinds_rec []       []    acc = (reverse acc,Nothing)
groupBinds_rec (a:as)   []    acc = (reverse acc,Just $ OtherError msg)
    where msg =    text "Could not find any bindings." 
                $$ text "Is the second pass placed after main simplifier phases ?" 
groupBinds_rec []       binds acc = (reverse acc,Just $ OrderingFailedBinds (length acc) binds)
groupBinds_rec (tg:tgs) binds acc = do
    let predicate (NonRec id _) = case getGStorableType $ varType id of
            Just t -> t `elemType` tg
            Nothing -> False
        predicate (Rec _) = False
    let (layer, rest) = partition predicate binds
    if length layer == 0 
        then (reverse acc, Just $ OrderingFailedBinds (length acc) rest)
        else groupBinds_rec tgs rest (reverse layer:acc)
