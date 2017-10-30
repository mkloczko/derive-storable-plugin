{-|
Module      : Foreign.Storable.Generic.Plugin.Internal.Printer
Copyright   : (c) Mateusz Kłoczko, 2017
License     : MIT
Maintainer  : mateusz.p.kloczko@gmail.com
Stability   : experimental
Portability : GHC-only

Printers.

-}

module Foreign.Storable.Generic.Plugin.Internal.Printer where

import GHC
import Var
import TyCoRep

import CoreMonad
import Outputable

printId :: Id -> CoreM ()
printId id
    | isId id
    = printType (varType id)
    | otherwise = return ()

printType :: Type -> CoreM ()
printType (TyVarTy var) = putMsg $ text "tyvarty"
printType (AppTy t1 t2) = putMsg $ text "application of a type"
printType (TyConApp tc k) = putMsg $ text "ty con app"
printType (ForAllTy tvbindr t) = putMsg $ text "for all type"
printType (FunTy    t1 t2) = putMsg $ text "fun ty"
printType (LitTy    tl)    = putMsg $ text "literal type"
printType (CastTy   t kc) = putMsg $ text "cast type"
printType (CoercionTy c) = putMsg $ text "coercion"


-- Type
--  = TyVarTy Var
--  | AppTy Type Type
--  | TyConApp TyCon.TyCon [KindOrType]
--  | ForAllTy {-# UNPACK #-}TyVarBinder Type
--  | FunTy Type Type
--  | LitTy TyLit
--  | CastTy Type KindCoercion
--  | CoercionTy Coercion

