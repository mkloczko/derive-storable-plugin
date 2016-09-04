module Foreign.Storable.Generic.Plugin.Internal.Predicates where

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
-- Types 
import Type (isAlgType, splitTyConApp_maybe)
import TyCon (algTyConRhs, visibleDataCons)
import TyCoRep (Type(..), TyBinder(..))
import TysWiredIn (intDataCon)
import DataCon    (dataConWorkId,dataConOrigArgTys) 

import MkCore (mkWildValBinder)
-- Printing
import Outputable (cat, ppr, SDoc, showSDocUnsafe)
import CoreMonad (putMsg, putMsgS)


{- Somehow get types, monsieur!
Do some type-level checking here.

Maybe later ? Just get the basic case running.
Mkay!


-}

import Data.Maybe 

import Foreign.Storable.Generic.Plugin.Internal.Helpers




-- | Predicate used to find GStorable instances identifiers.
isGStorableInstId :: Id -> Bool
isGStorableInstId id = cutted_occ_name == gstorable_dict_name
    where cutted_occ_name = cutOccName 11 $ getOccName (varName id)
          gstorable_dict_name = mkOccName N.varName "$fGStorable"

-- | Predicate used to find gsizeOf identifiers
isSizeOfId :: Id -> Bool
isSizeOfId ident = getOccName (varName ident)    == mkOccName N.varName "$cgsizeOf" 

-- | Predicate used to find galignment identifiers
isAlignmentId :: Id -> Bool
isAlignmentId ident = getOccName (varName ident) == mkOccName N.varName "$cgalignment" 

-- | Predicate used to find gpeekByteOff identifiers
isPeekId :: Id -> Bool
isPeekId ident = getOccName (varName ident) == mkOccName N.varName "$cgpeekByteOff" 

-- | Predicate used to find gpeekByteOff identifiers
isPokeId :: Id -> Bool
isPokeId ident = getOccName (varName ident) == mkOccName N.varName "$cgpokeByteOff" 

--------------------------------------------
--Specialized at instance definition site.--
--------------------------------------------

-- -- | Predicate used to find gsizeOf identifiers
-- isSpecGStorableDictId :: Id -> Bool
-- isSpecGStorableDictId id = cutted_occ_name == gstorable_dict_name
--     where cutted_occ_name = cutOccName 13 $ getOccName (varName id)
--           gstorable_dict_name = mkOccName N.varName "$s$fGStorable"
-- 
-- -- | Predicate used to find gsizeOf identifiers
-- isSpecSizeOfId :: Id -> Bool
-- isSpecSizeOfId ident = getOccName (varName ident)    == mkOccName N.varName "$s$cgsizeOf" 
-- 
-- -- | Predicate used to find galignment identifiers
-- isSpecAlignmentId :: Id -> Bool
-- isSpecAlignmentId ident = getOccName (varName ident) == mkOccName N.varName "$s$cgalignment" 
-- 
-- -- | Predicate used to find gpeekByteOff identifiers
-- isSpecPeekId :: Id -> Bool
-- isSpecPeekId ident = getOccName (varName ident) == mkOccName N.varName "$s$cgpeekByteOff" 
-- 
-- -- | Predicate used to find gpeekByteOff identifiers
-- isSpecPeekId :: Id -> Bool
-- isSpecPeekId ident = getOccName (varName ident) == mkOccName N.varName "$s$cgpeekByteOff" 


----------------------------
-- For offset calculation --
----------------------------

isOffsetsId :: Id -> Bool
isOffsetsId id = getOccName (varName id) == mkOccName N.varName "offsets"

---------------------------
-- Groups of identifiers --
---------------------------

-- | Is a GStorable identifier
isAnyGStorable :: Id -> Bool
isAnyGStorable id = any ($id) [isSizeOfId, isAlignmentId, isPeekId
                              , isPokeId, isGStorableInstId
                              ]
-- | Is the id an GStorable method.
isGStorableMethodId :: Id -> Bool 
isGStorableMethodId id = any ($id) [isSizeOfId, isAlignmentId
                                   , isPeekId, isPokeId
                                   ]
------------------                                   
-- Miscellanous --
------------------

isNonRecBind :: CoreBind -> Bool
isNonRecBind (NonRec _ _) = True
isNonRecBind _            = False

toIsBind :: (Id -> Bool) -> CoreBind -> Bool
toIsBind pred (NonRec id rhs) = pred id
toIsBind pred (Rec bs)        = any pred $ map fst bs
 
withTypeCheck :: (Type -> Maybe Type) -> (Id -> Bool) -> Id -> Bool
withTypeCheck ty_f id_f id = do
    let ty_checked = ty_f $ varType id
        id_checked = id_f id
    and [isJust ty_checked, id_checked]
