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

printId :: Id -> SDoc
printId id
    | isId id
    = printType (varType id)
    | otherwise = empty

printType :: Type -> SDoc
printType (TyVarTy var) = printVar var
printType (AppTy t1 t2) = printType t1 <+> text "and" <+> printType t2
printType (TyConApp tc k) = ppr tc <+> sep (map printType k)
printType (ForAllTy tvbindr t) = text "forall." <+> printTyVarBinder tvbindr <+> printType t
printType (FunTy    t1 t2) = printType t1 <+> text "->" <+> printType t2
printType (LitTy    tl)    = text "literal type"
printType (CastTy   t kc) = text "cast type"
printType (CoercionTy c) = text "coercion"


printTyVarBinder :: TyVarBinder -> SDoc
printTyVarBinder tyvarbndr = printVar (binderVar tyvarbndr) <+> ppr (binderArgFlag tyvarbndr)


printTyLit :: TyLit -> SDoc
printTyLit (NumTyLit int) = text "NumberType"
printTyLit (StrTyLit str) = text "StringType"

printVar :: Var -> SDoc
printVar var
    | isTyVar   var
    = ppr (varName var)
    | isTcTyVar var
    = text "type contructor type"
    | isId      var
    = text "id"
-- Type
--  = TyVarTy Var
--  | AppTy Type Type
--  | TyConApp TyCon.TyCon [KindOrType]
--  | ForAllTy {-# UNPACK #-}TyVarBinder Type
--  | FunTy Type Type
--  | LitTy TyLit
--  | CastTy Type KindCoercion
--  | CoercionTy Coercion

