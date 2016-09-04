module Foreign.Storable.Generic.Plugin.Internal.Types where

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
import TyCon (TyCon(..),algTyConRhs, visibleDataCons)
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
import Control.Applicative
import Control.Monad.IO.Class


import Foreign.Storable.Generic.Plugin.Internal.Helpers


-- | Function for getting types from an id.
import TyCon      (isUnboxedTupleTyCon)
import TysWiredIn (intTyCon, constraintKind, constraintKindTyCon, listTyCon, intTy)
import PrelNames  (ioTyConKey, ptrTyConKey, realWorldTyConKey, statePrimTyConKey)
import Type       (isUnboxedTupleType)


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

-- | Check whether the type is of kind * -> Constraint.
hasConstraintKind :: Type -> Bool
hasConstraintKind ty 
    | TyConApp tc   [a]     <- ty
    , ForAllTy star kind_ty <- tyConKind tc
    , TyConApp k_tc []      <- kind_ty
    = constraintKindTyCon == k_tc
    | otherwise = False

-- | Check whether the 


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
    | ForAllTy ty_bind int_t <- t
    , isIntType int_t 
    , Anon the_t <- ty_bind
    = Just the_t
    | ForAllTy _ some_t <- t = getAlignmentType some_t
    | otherwise  = Nothing

-- | Get the type from GStorable sizeOf method
getSizeType :: Type -> Maybe Type
getSizeType t
    -- Assuming there are no anonymous ty bind between
    -- the type and the integer, ie no : Type -> forall a. Int
    | ForAllTy ty_bind int_t <- t
    , isIntType int_t 
    , Anon the_t <- ty_bind
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
    , ForAllTy ty_bind io_t <- t
    , Anon int_t <- ty_bind
    , isIntType int_t
    = getPeekType' io_t True True
    -- Ptr b -> Int -> IO (TheType)
    | ForAllTy ty_bind int_t <- t
    , Anon ptr_t <- ty_bind
    , isPtrType ptr_t
    = getPeekType' int_t True False
    -- Ignore other types
    -- including constraints and 
    -- Named ty binders.
    | ForAllTy _ some_t <- t
    = getPeekType' some_t after_ptr after_int
    | otherwise = Nothing



-- -- | Get the type from GStorable peek method
-- getPeekPrimType :: Type -> Maybe Type
-- getPeekPrimType t = getPeekPrimType' t False False False
-- 
-- -- | Insides of getPeekType, which takes into the account
-- -- the order of arguments.
-- getPeekPrimType' :: Type 
--              -> Bool -- ^ Is after Ptr
--              -> Bool -- ^ Is after Int
--              -> Bool -- ^ Is after State# 
--              -> Maybe Type -- ^ Returning
-- getPeekPrimType' t after_ptr after_int after_state
--     -- Last step: (# State# RealWorld, Type #)
--     | after_ptr, after_int, after_state
--     -- , TyConApp unbx_tpl [state, the_t] <- t
--     -- , isUnboxedTupleTyCon unbx_tpl
--     -- , isStatePrimType state
--     = trace ("got the type: " ++ showSDocUnsafe (ppr t)) $ Nothing -- $ Just the_t
--     -- State# -> (# State# RealWorld, Type #)
--     | after_ptr, after_int
--     , ForAllTy st_bind last <- t
--     , Anon st_t <- st_bind
--     , isStatePrimType st_t
--     = trace "Got the state#" $ getPeekPrimType' last True True True
--     -- Int -> State# RealWorld -> (# State# RealWorld, Type #)
--     | after_ptr
--     , ForAllTy int_bind rest <- t
--     , Anon int_t <- int_bind
--     , isIntType int_t
--     = trace "got the smth:" $ getPeekPrimType' rest True True False
--     -- Ptr b -> Int -> State# RealWorld -> (# State# RealWorld , Type #)
--     | ForAllTy ptr_bind int_t <- t
--     , Anon ptr_t <- ptr_bind
--     , isPtrType ptr_t
--     = getPeekPrimType' int_t True False False
--     -- Ignore other types
--     -- including constraints and 
--     -- Named ty binders.
--     | ForAllTy _ some_t <- t
--     = getPeekPrimType' some_t after_ptr after_int after_state
--     | otherwise = Nothing

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
    , ForAllTy ty_bind io_t <- t
    , isIOType io_t
    , Anon the_t <- ty_bind
    = Just the_t
    -- Int -> TheType -> IO ()
    | after_ptr
    , ForAllTy ty_bind rest <- t
    , Anon int_t <- ty_bind
    , isIntType int_t
    = getPokeType' rest True True
    -- Ptr b -> Int -> TheType -> IO ()
    | ForAllTy ty_bind int_rest <- t
    , Anon ptr_t <- ty_bind
    , isPtrType ptr_t
    = getPokeType' int_rest True False
    -- Ignore other types
    -- including constraints and 
    -- Named ty binders.
    | ForAllTy _ some_t <- t
    = getPokeType' some_t after_ptr after_int
    | otherwise = Nothing


getOffsetsType :: Type -> Maybe Type
getOffsetsType ty
    | TyConApp list_tc [int_t] <- ty
    , listTyCon == list_tc
    , intTy `eqType` int_t
    = Just ty
    | otherwise = Nothing

-- Combination of all above:
getGStorableType :: Type -> Maybe Type
getGStorableType t = getGStorableInstType t <|> getSizeType t <|> getAlignmentType t <|> getPokeType t <|> getPeekType t

-- Combination of methods.
getGStorableMethodType :: Type -> Maybe Type
getGStorableMethodType t = getSizeType t <|> getAlignmentType t <|> getPokeType t <|> getPeekType t
-- getOptimizableType :: Type -> Maybe Type
-- getOptimizableType t = getSizeType t <|> getAlignmentType t <|> getPeekPrimType t
