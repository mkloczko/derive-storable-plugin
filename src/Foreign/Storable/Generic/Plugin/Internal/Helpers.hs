{-|
Module      : Foreign.Storable.Generic.Plugin.Internal.Helpers
Copyright   : (c) Mateusz Kłoczko, 2016
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : portable

Various helping functions.

-}
module Foreign.Storable.Generic.Plugin.Internal.Helpers where

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


-- | Get ids from core bind.
getIdsBind :: CoreBind -> [Id]
getIdsBind (NonRec id _) = [id]
getIdsBind (Rec recs)    = map fst recs

-- | Get all expressions from a binding.
getExprsBind :: CoreBind -> [CoreExpr]
getExprsBind (NonRec _ e) = [e]
getExprsBind (Rec   recs) = map snd recs

-- | Get both identifiers and expressions from a binding.
getIdsExprsBind :: CoreBind -> [(Id,CoreExpr)]
getIdsExprsBind (NonRec id expr) = [(id,expr)]
getIdsExprsBind (Rec       recs) = recs

-- | Get all IDs from CoreExpr
getIdsExpr :: CoreExpr -> [Id]
getIdsExpr (Var id)    = [id]
getIdsExpr (App e1 e2) = concat [getIdsExpr e1, getIdsExpr e2]
getIdsExpr (Lam id e)  = id : getIdsExpr e
-- Ids from bs are ignored, as they are supposed to appear in e argument.
getIdsExpr (Let bs e)  = concat [getIdsExpr e, concatMap getIdsExpr (getExprsBind bs)]
-- The case_binder is ignored - the evaluated expression might appear on the rhs of alts
getIdsExpr (Case e _ _ alts) = concat $ getIdsExpr e : map (\(_,_,e_c) -> getIdsExpr e_c) alts
getIdsExpr (Cast e _) = getIdsExpr e 
getIdsExpr _           = []


------------
-- others --
------------


-- | Takes first n characters out of occName
cutOccName :: Int -> OccName -> OccName
cutOccName n occ_name = mkOccName (occNameSpace occ_name) name_string
    where name_string = take n $ occNameString occ_name


-- HACK for type equality
-- | Equality for types
eqType :: Type -> Type -> Bool
eqType (TyVarTy v1) (TyVarTy v2) = v1 == v2
eqType (AppTy t1a t1b) (AppTy t2a t2b) = t1a `eqType` t2a && t1b `eqType` t2b
eqType (TyConApp tc1 ts1) (TyConApp tc2 ts2) = tc1 == tc2 && (and $ zipWith eqType ts1 ts2)
eqType (ForAllTy tb1 t1)  (ForAllTy tb2 t2)  = tb1 `eqTyBind` tb2 && t1 `eqType` t2
-- Not dealing with type coercions or casts.
eqType _ _                     = False

-- | Equality for type binders
eqTyBind :: TyBinder -> TyBinder -> Bool
eqTyBind (Named t1 vis1) (Named t2 vis2) = t1 == t2 && vis1 == vis2
eqTyBind (Anon t1) (Anon t2) = t1 `eqType` t2
eqTyBind _ _ = False

-- | 'elem' function for types
elemType :: Type -> [Type] -> Bool
elemType t [] = False
elemType t (ot:ts) = (t `eqType` ot) || elemType t ts
