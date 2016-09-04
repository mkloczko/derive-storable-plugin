module Foreign.Storable.Generic.Plugin.Internal.GroupTypes where

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
import CoreMonad (putMsg, putMsgS)

-- Used to get to compiled values
import GHCi.RemoteTypes



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

    if length layer == 0
        then (reverse acc, Just $ OrderingFailedTypes (length acc) rest)
        else calcGroupOrder_rec rest (layer:acc)

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

-- | Get data constructor arguments from an algebraic type.
getDataConArgs :: Type -> [Type]
getDataConArgs t 
    | isAlgType t
    -- TODO: Inspect the arguments of type constructor
    -- Perhaps one could put them inside the 
    , Just (tc, _) <- splitTyConApp_maybe t
    = concatMap dataConOrigArgTys $ (visibleDataCons.algTyConRhs) tc
    -- TODO: Handle type synonyms and type family instances
    | otherwise = []


-- groupBinds :: [[Type]] -> [CoreBind] -> Either Error [[CoreBind]]
-- compileGroups :: Flags -> [[CoreBind]] -> IO [CoreBind]
-- grouping_errors :: Flags -> [Error] -> CoreM [CoreBind]

groupBinds :: [[Type]]   -- ^ Type groups.
           -> [CoreBind] -- ^ Should be only NonRecs. 
           -> ([[CoreBind]], Maybe Error)
-- perhaps add some safety so non-recs won't get here.
groupBinds type_groups binds = groupBinds_rec type_groups binds []
        
groupBinds_rec :: [[Type]] 
               -> [CoreBind]
               -> [[CoreBind]]
               -> ([[CoreBind]], Maybe Error)
groupBinds_rec []       []    acc = (reverse acc,Nothing)
groupBinds_rec []       binds acc = (reverse acc,Just $ OrderingFailedBinds (length acc) binds)
groupBinds_rec (tg:tgs) binds acc = do
    let predicate (NonRec id _) = case getGStorableType $ varType id of
            Just t -> t `elemType` tg
            Nothing -> False
        predicate (Rec _) = False
    let (layer, rest) = partition predicate binds
    if length layer == 0 
        then (reverse acc, Just $ OrderingFailedBinds (length acc) rest)
        else groupBinds_rec tgs rest (layer:acc)