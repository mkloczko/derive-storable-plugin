{-|
Module      : Foreign.Storable.Generic.Plugin.Internal.Types
Copyright   : (c) Mateusz Kłoczko, 2016
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : GHC-only

Functions for obtaining types from GStorable methods and instances.

-}
{-#LANGUAGE CPP #-}
module Foreign.Storable.Generic.Plugin.Internal.Types
    (
    -- Type predicates
      isIntType
    , isPtrType
    , isIOType
    , isIOTyCon
    , isStatePrimType
    , isStatePrimTyCon
    , isRealWorldType
    , isRealWorldTyCon
    , isGStorableInstTyCon
    , hasConstraintKind
    , hasGStorableConstraints
    -- Used to obtain types
    , getGStorableInstType
    , getAlignmentType
    , getSizeType
    , getPeekType
    , getPokeType
    , getOffsetsType
    -- Combinations of above
    , getGStorableType
    , getGStorableMethodType
    )
    where




#if MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
import GHC.Core          (Bind(..),Expr(..), CoreExpr, CoreBind, CoreProgram, Alt)
import GHC.Types.Literal (Literal(..))
import GHC.Types.Id      (isLocalId, isGlobalId,Id)
import GHC.Types.Var             (Var(..))
import GHC.Types.Name            (getOccName,mkOccName)
import GHC.Types.Name.Occurrence (OccName(..), occNameString)
import qualified GHC.Types.Name as N (varName, tcClsName)
import GHC.Types.SrcLoc (noSrcSpan)
import GHC.Types.Unique (getUnique)
import GHC.Driver.Main (hscCompileCoreExpr, getHscEnv)
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
import GHC.Driver.Env.Types (HscEnv)
import GHC.Unit.Module.ModGuts (ModGuts(..))
#else
import GHC.Driver.Types (HscEnv,ModGuts(..))
#endif
import GHC.Core.Opt.Monad (CoreM,CoreToDo(..))
import GHC.Types.Basic (CompilerPhase(..))
import GHC.Core.Type (isAlgType, splitTyConApp_maybe)
import GHC.Core.TyCon (TyCon(..),algTyConRhs, visibleDataCons)
import GHC.Builtin.Types   (intDataCon)
import GHC.Core.DataCon    (dataConWorkId,dataConOrigArgTys) 
import GHC.Core.Make       (mkWildValBinder)
import GHC.Utils.Outputable (cat, ppr, SDoc, showSDocUnsafe)
import GHC.Core.Opt.Monad (putMsg, putMsgS)
#elif MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
import CoreSyn (Bind(..),Expr(..), CoreExpr, CoreBind, CoreProgram, Alt)
import Literal (Literal(..))
import Id  (isLocalId, isGlobalId,Id)
import Var (Var(..))
import Name (getOccName,mkOccName)
import OccName (OccName(..), occNameString)
import qualified Name as N (varName, tcClsName)
import SrcLoc (noSrcSpan)
import Unique (getUnique)
import HscMain (hscCompileCoreExpr)
import HscTypes (HscEnv,ModGuts(..))
import CoreMonad (CoreM,CoreToDo(..), getHscEnv)
import BasicTypes (CompilerPhase(..))
import Type (isAlgType, splitTyConApp_maybe)
import TyCon (TyCon(..),algTyConRhs, visibleDataCons)
import TysWiredIn (intDataCon)
import DataCon    (dataConWorkId,dataConOrigArgTys) 
import MkCore (mkWildValBinder)
import Outputable (cat, ppr, SDoc, showSDocUnsafe)
import CoreMonad (putMsg, putMsgS)
#endif

import GHCi.RemoteTypes

import Unsafe.Coerce

import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace
import Control.Applicative
import Control.Monad.IO.Class


import Foreign.Storable.Generic.Plugin.Internal.Helpers


-- Function for getting types from an id.
#if MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
import GHC.Core.TyCon      (isUnboxedTupleTyCon)
import GHC.Builtin.Types (intTyCon, constraintKind, constraintKindTyCon, listTyCon, intTy)
import GHC.Builtin.Names  (ioTyConKey, ptrTyConKey, realWorldTyConKey, statePrimTyConKey)
import GHC.Core.Type       (isUnboxedTupleType)
#else
import TyCon      (isUnboxedTupleTyCon)
import TysWiredIn (intTyCon, constraintKind, constraintKindTyCon, listTyCon, intTy)
import PrelNames  (ioTyConKey, ptrTyConKey, realWorldTyConKey, statePrimTyConKey)
import Type       (isUnboxedTupleType)
#endif


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

-- | Check whether the type is integer
isIntType :: Type -> Bool
isIntType (TyConApp int []) = int == intTyCon
isIntType _                 = False

-- | Check whether the type is a Pointer
isPtrType :: Type -> Bool
isPtrType (TyConApp ptr [el]) = getUnique ptr == ptrTyConKey
isPtrType _                   = False


-- | Check whether the type is a IO.
isIOType :: Type -> Bool
isIOType (TyConApp io [el]) = isIOTyCon io
isIOType _                  = False

-- | Check whether the type constructor is an IO.
isIOTyCon :: TyCon -> Bool
isIOTyCon io = getUnique io == ioTyConKey

-- | Check whether the type is a State#
isStatePrimType :: Type -> Bool
isStatePrimType (TyConApp st [el]) = isStatePrimTyCon st
isStatePrimType  _                 = False

-- | Check whether the type constructor is a State#
isStatePrimTyCon :: TyCon -> Bool
isStatePrimTyCon st = getUnique st == statePrimTyConKey

-- | Check whether the type is a RealWorld#
isRealWorldType :: Type -> Bool
isRealWorldType (TyConApp rw []) = isRealWorldTyCon rw
isRealWorldType _                = False

-- | Check whether the type constructor is a RealWorld#
isRealWorldTyCon :: TyCon -> Bool
isRealWorldTyCon rw = getUnique rw == realWorldTyConKey

-- | Check whether the type is a State# RealWorld.
isStateRealWorld :: Type -> Bool
isStateRealWorld t@(TyConApp st [rl]) = isStatePrimType t && isRealWorldType rl
isStateRealWorld _ = False

-- | Check whether the type constuctor is a GStorable
isGStorableInstTyCon :: TyCon -> Bool
isGStorableInstTyCon tc = getOccName (tyConName tc) == mkOccName N.tcClsName "GStorable" 

-- | Check whether the type is of kind * -> Constraint.
hasConstraintKind :: Type -> Bool
hasConstraintKind ty 
    | TyConApp tc   [a]     <- ty
    , ForAllTy star kind_ty <- tyConKind tc
    , TyConApp k_tc []      <- kind_ty
    = constraintKindTyCon == k_tc
    | otherwise = False

-- | Check whether the type has GStorable constraints.
hasGStorableConstraints :: Type -> Bool
hasGStorableConstraints t
    | ForAllTy bind next  <- t
#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
    , isId $ binderVar bind
    , gstorable_cons <- varType $ binderVar bind
#else
    , Anon gstorable_cons <- bind
#endif
    , hasConstraintKind gstorable_cons
    , TyConApp gstorable_tc [_] <- gstorable_cons
    , isGStorableInstTyCon gstorable_tc
    = True
    | ForAllTy _ next <- t
    = hasGStorableConstraints next
    | otherwise = False


-- | Get the type from GStorable instance.
getGStorableInstType :: Type -> Maybe Type
getGStorableInstType t
    | hasConstraintKind t
    , TyConApp gstorable [the_t] <- t 
    = Just the_t
    -- Ignore forall a. a, GStorable a =>, etc..
    | ForAllTy _ some_t <- t  = getGStorableInstType some_t
    | otherwise               = Nothing


-- | Get the type from GStorable alignment method
getAlignmentType :: Type -> Maybe Type
getAlignmentType t
    -- Assuming there are no anonymous ty bind between
    -- the type and the integer, ie no : Type -> forall a. Int
#if   MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
    | FunTy _ _ t1 t2 <- t
    -- , isIntType t2
    , TyConApp _ _ <- t2
    , the_t <- t1
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
    | FunTy _ t1 t2 <- t
    -- , isIntType t2
    , TyConApp _ _ <- t2
    , the_t <- t1
#elif MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
    | FunTy t1 t2 <- t
    , isIntType t2
    , the_t <- t1
#else
    | ForAllTy ty_bind int_t <- t
    , isIntType int_t
    , Anon the_t <- ty_bind
#endif
    = Just the_t
    | ForAllTy _ some_t <- t = getAlignmentType some_t
    | otherwise  = Nothing

-- | Get the type from GStorable sizeOf method
getSizeType :: Type -> Maybe Type
getSizeType t
    -- Assuming there are no anonymous ty bind between
    -- the type and the integer, ie no : Type -> forall a. Int
#if   MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
    | FunTy _ _ t1 t2 <- t
    -- , isIntType t2
    , TyConApp _ _ <- t2
    , the_t <- t1
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
    | FunTy _ t1 t2 <- t
    -- , isIntType t2
    , TyConApp _ _ <- t2
    , the_t <- t1
#elif MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
    | FunTy t1 t2 <- t
    , isIntType t2
    , the_t <- t1
#else
    | ForAllTy ty_bind int_t <- t
    , isIntType int_t
    , Anon the_t <- ty_bind
#endif
    = Just the_t
    | ForAllTy _ some_t <- t = getSizeType some_t
    | otherwise  = Nothing




-- | Get the type from GStorable peek method
getPeekType :: Type -> Maybe Type
getPeekType t = getPeekType' t False False

-- | Insides of getPeekType, which takes into the account
-- the order of arguments.
getPeekType' :: Type 
             -> Bool -- ^ Is after Ptr
             -> Bool -- ^ Is after Int
             -> Maybe Type -- ^ Returning
getPeekType' t after_ptr after_int 
    -- Last step: IO (TheType)
    | after_ptr, after_int
    , TyConApp io_tc [the_t] <- t
    , isIOTyCon io_tc
    = Just the_t
    -- Int -> IO (TheType)
    | after_ptr
#if   MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
    , FunTy _ _ int_t io_t <- t
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
    , FunTy _ int_t io_t <- t
#elif MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
    , FunTy int_t io_t <- t
#else
    , ForAllTy ty_bind io_t <- t
    , Anon int_t <- ty_bind
#endif
    , isIntType int_t
    = getPeekType' io_t True True
    -- Ptr b -> Int -> IO (TheType)
#if   MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
    | ForAllTy ty_bind fun_t <- t
    , FunTy _ _ ptr_t rest <- fun_t 
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
    | ForAllTy ty_bind fun_t <- t
    , FunTy _ ptr_t rest <- fun_t 
#elif MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
    | ForAllTy ty_bind fun_t <- t
    , FunTy ptr_t rest <- fun_t 
#else
    | ForAllTy ty_bind rest <- t
    , Anon ptr_t <- ty_bind
#endif
    , isPtrType ptr_t
    = getPeekType' rest True False
    -- Ignore other types
    -- including constraints and 
    -- Named ty binders.
    | ForAllTy _ some_t <- t
    = getPeekType' some_t after_ptr after_int
    | otherwise = Nothing



--isUnboxedTuple2 is State# h 

-- | Get the type from GStorable poke method
getPokeType :: Type -> Maybe Type
getPokeType t = getPokeType' t False False

getPokeType' :: Type 
             -> Bool -- ^ Is after Ptr
             -> Bool -- ^ Is after Int
             -> Maybe Type -- ^ Returning
getPokeType' t after_ptr after_int 
    -- Last step: TheType -> IO ()
    | after_ptr, after_int
#if MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
    , FunTy _ _ the_t io_t <- t
    , isIOType io_t
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
    , FunTy _ the_t io_t <- t
    , isIOType io_t
#elif MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
    , FunTy the_t io_t <- t
    , isIOType io_t
#else
    , ForAllTy ty_bind io_t <- t
    , isIOType io_t
    , Anon the_t  <- ty_bind
#endif
    = Just the_t
    -- Int -> TheType -> IO ()
    | after_ptr
#if   MIN_VERSION_GLASGOW_HASKELL(9, 0,1,0)
    , FunTy _ _ int_t rest <- t
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
    , FunTy _ int_t rest <- t
#elif MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
    , FunTy int_t rest <- t
#else
    , ForAllTy ty_bind rest <- t
    , Anon int_t <- ty_bind
#endif
    , isIntType int_t
    = getPokeType' rest True True
    -- Ptr b -> Int -> TheType -> IO ()
#if   MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
    | ForAllTy ty_bind fun_t <- t
    , FunTy _ _ ptr_t rest <- fun_t
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
    | ForAllTy ty_bind fun_t <- t
    , FunTy _ ptr_t rest <- fun_t
#elif MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
    | ForAllTy ty_bind fun_t <- t
    , FunTy ptr_t rest <- fun_t
#else
    | ForAllTy ty_bind rest <- t
    , Anon ptr_t <- ty_bind
#endif 
    , isPtrType ptr_t
    = getPokeType' rest True False
    -- Ignore other types
    -- including constraints and 
    -- Named ty binders.
    | ForAllTy _ some_t <- t
    = getPokeType' some_t after_ptr after_int
    | otherwise = Nothing


-- | Get the type of Offsets. Assuming it is [Int]
getOffsetsType :: Type -> Maybe Type
getOffsetsType ty
    | TyConApp list_tc [int_t] <- ty
    , listTyCon == list_tc
    , intTy `eqType` int_t
    = Just ty
    | otherwise = Nothing

-- | Combination of type getters for all GStorables.
getGStorableType :: Type -> Maybe Type
getGStorableType t' = getGStorableInstType t <|> getSizeType t <|> getAlignmentType t <|> getPokeType t <|> getPeekType t
    where t = removeProxy t'

-- | Combination of type getters for GStorable methods.
getGStorableMethodType :: Type -> Maybe Type
getGStorableMethodType t = getSizeType t <|> getAlignmentType t <|> getPokeType t <|> getPeekType t
