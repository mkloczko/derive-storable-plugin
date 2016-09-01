{-# LANGUAGE Strict #-}
module Foreign.Storable.Generic.Plugin.Internal.InstanceDecl where

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

import Foreign.Storable.Generic.Plugin.Internal hiding (ErrorMsg(..))
import qualified Foreign.Storable.Generic.Plugin.SimplCore as SC

data ErrorMsg = OrderingFailed Int [CoreBindTypes] 
              | DictErr DictProblem CoreBind 
              | TypeErr Type String
              | Other String
data DictProblem = NoDict | MultipleDicts 

-- | Takes first n characters out of occName
cutOccName :: Int -> OccName -> OccName
cutOccName n occ_name = mkOccName (occNameSpace occ_name) name_string
    where name_string = take n $ occNameString occ_name

-- | Predicate used to find gsizeOf identifiers
-- Note to self - perhaps export OccName.NameSpace constructors ?
-- varName is just a wrapper over VarName constructor..
isGStorableDictId :: Id -> Bool
isGStorableDictId id = cutted_occ_name == gstorable_dict_name
    where cutted_occ_name = cutOccName 11 $ getOccName (varName id)
          gstorable_dict_name = mkOccName N.varName "$fGStorable"



-- | Orders GStorable bindings, which should have format:
--   Rec [($fGStorableType, expr1)
--       ,($cgsizeOf,       expr2)
--       ,($cgalignment,    expr3)
--       ,($cgpeekByteOff,  expr4)
--       ,($cgpokeByteOff,  expr5)
--       ]
-- orderByDeps :: [(a, Type)] -> [(a,Type)] 
-- orderByDeps as = do 
    -- First should be types that have no adts

-- | Check whether the binding is compileable by checking the ids in expression.
okToCompile :: CoreBind -> Bool
okToCompile core_bind = not $ any isLocalId ids_in_exprs
   where ids_in_exprs = concatMap getIdsExpr $ getExprsBind core_bind 

-- | Compile the expression in Core Bind and replace it.
compileGStorableBind :: CoreBind -> CoreM CoreBind 
compileGStorableBind core_bind
    -- Substitute gsizeOf
    | (NonRec id expr) <- core_bind
    , isSizeOfId core_bind  
    = getHscEnv >>= \x -> liftIO $ intSubstitution x core_bind
    -- Substitute galignment
    | (NonRec id expr) <- core_bind
    , isAlignmentId core_bind   
    = getHscEnv >>= \x -> liftIO $ intSubstitution x core_bind
    -- Everything else - nope.
    -- Perhaps warn/crash.
    | otherwise = return core_bind

nonRec :: CoreBind -> Bool
nonRec (NonRec _ _) = True
nonRec _            = False

-- | Simplify, substitute local ids, and compile GStorable expressions.
simplifySubstituteCompile :: ModGuts -> [[CoreBind]] -> [CoreBind] -> [CoreBind] -> CoreM (ModGuts,[CoreBind],[CoreBind])
simplifySubstituteCompile guts  []                 substituted not_subs = return (guts, substituted, not_subs) 
simplifySubstituteCompile guts (layer:more_nested) substituted not_subs = do
    let guts_layer = guts { mg_binds = layer }
        -- Simplifier parameters: two iterations, do rules and inlining 
        simpl_todo = CoreDoSimplify 3 $ SimplMode ["Simplifying GStorable methods"] (Phase (2)) True True False False 
    
    guts_simpl <- SC.simplifyPgm simpl_todo guts_layer
    
        -- Simplified bindings from the layer
    let layer_simpl   = mg_binds guts_simpl
        -- Possible substitutions - either from simplified layer or from earlier layers.
        possible_subs = filter nonRec ( concat [layer_simpl, substituted])
    
        layer' = map (replaceIdsBind possible_subs) layer_simpl
    putMsgS $ "Layer?! " ++ show (length layer')
    -- putMsg $ ppr layer'
        -- Which are ok to compile - all ids in expression have to be global.
        -- WRONG - some are local - like in cases, etc...
        -- Maybe just compile and let it crash ?..
    let (to_comp, to_leave) = partition okToCompile layer'
    -- Do the compilation
    new_binds <- mapM compileGStorableBind layer'

    simplifySubstituteCompile guts_simpl more_nested (concat [new_binds, substituted]) (concat  [not_subs])


-- | Get data constructor types from a type.
getDataConArgsType :: Type -> [Type]
getDataConArgsType t 
    | isAlgType t
    , Just (tc, _) <- splitTyConApp_maybe t
    = concatMap dataConOrigArgTys $ (visibleDataCons.algTyConRhs) tc
    -- TODO: Handle type synonyms and type family instances
    | otherwise = []

type CoreBindTypes = (CoreBind, Type, [Type])

groupByTypes :: [CoreBindTypes] -> ([[CoreBind]], Maybe ErrorMsg)
groupByTypes gd_binds = groupByTypes_rec gd_binds []


groupByTypes_rec :: [CoreBindTypes] -> [[CoreBind]] -> ([[CoreBind]], Maybe ErrorMsg)
groupByTypes_rec []       acc = (reverse acc, Nothing)
groupByTypes_rec gd_binds acc = do
    let (layer, rest) = groupByTypes_inside gd_binds [] [] []
        
    if length layer == 0 
        -- If the ordering was performed and no new layers
        -- were found, the ordering went wrong.
        then (reverse acc, Just $ OrderingFailed (length acc) rest)
        else groupByTypes_rec rest (layer:acc) 

-- HACK for type equality
eqType :: Type -> Type -> Bool
eqType (TyVarTy v1) (TyVarTy v2) = v1 == v2
eqType (AppTy t1a t1b) (AppTy t2a t2b) = t1a `eqType` t2a && t1b `eqType` t2b
eqType (TyConApp tc1 ts1) (TyConApp tc2 ts2) = tc1 == tc2 && (and $ zipWith eqType ts1 ts2)
eqType (ForAllTy tb1 t1)  (ForAllTy tb2 t2)  = tb1 `eqTyBind` tb2 && t1 `eqType` t2
-- Not dealing with type coercions or casts.
eqType _ _                     = False

eqTyBind :: TyBinder -> TyBinder -> Bool
eqTyBind (Named t1 vis1) (Named t2 vis2) = t1 == t2 && vis1 == vis2
eqTyBind (Anon t1) (Anon t2) = t1 `eqType` t2
eqTyBind _ _ = False

elemType :: Type -> [Type] -> Bool
elemType t [] = False
elemType t (ot:ts) = (t `eqType` ot) || elemType t ts

-- | This could be done more efficently if we'd 
-- represent the problem as a graph problem.
groupByTypes_inside :: [CoreBindTypes]  -- ^ CoreBinding, it's type and type arguments
                    -> [CoreBindTypes]  -- ^ Those which were checked and not chosen to be for this layer
                    -> [CoreBind]       -- ^ Accumulator for bindings in this layer
                    -> [CoreBindTypes]  -- ^ Those which were checked and not chosen to be for this layer
                    -> ([CoreBind], [CoreBindTypes]) -- ^ In this layer and for the other.
groupByTypes_inside []                checked acc acc_later = (acc,acc_later)
groupByTypes_inside (x@(b,t,args):xs) checked acc acc_later = do
   -- Are the x's binding data constructor arguments used by some other type?
   let is_arg_somewhere = any (\(_,t,_) -> elemType t args) (concat [checked,xs])
       
   -- Yes - go for next layer.
   -- No  - go for this layer.
   if is_arg_somewhere 
      then groupByTypes_inside xs (x:checked) acc     (x:acc_later)
      else groupByTypes_inside xs (x:checked) (b:acc) acc_later    


-- Action -- 

getType :: Type -> Maybe Type
getType (TyConApp _ [t]) = Just t
getType otherwise   = Nothing

addTypes :: CoreBind -> Either ErrorMsg CoreBindTypes
addTypes core_bind = do
    -- List of continuations..
    -- Whether we get a dict or not.
    let is_one_dict  = \dicts -> case dicts of
            [dict] -> Right dict
            []     -> Left $ DictErr NoDict core_bind
            _      -> Left $ DictErr MultipleDicts core_bind
    -- Whether we get a type for dict
        has_app = \dict -> case getType $ varType dict of
            Nothing -> Left $ TypeErr (varType dict) "getType: Wrong pattern match"
            Just  t -> Right t
    -- Starting point for continuations
        dicts    = filter isGStorableDictId $ getIdsBind core_bind
    -- Get the type, and possibly the constructor arguments
    t    <- has_app  =<< is_one_dict dicts
    let args = getDataConArgsType t
    Right (core_bind, t, args)

actionFun :: ModGuts -> CoreM ModGuts 
actionFun guts = do
    let binds = mg_binds guts
        (gstorable,other) = partition ((any isGStorableDictId).getIdsBind) binds
        (err_msgs, core_bind_types) = partitionEithers $ map addTypes gstorable
        (layered, m_err)    = groupByTypes core_bind_types
    -- debuggy
    let tts = map (\(_,t,ts) -> (t,ts)) core_bind_types

    -- putMsgS "gstorables"
    -- putMsg $ ppr $ map getIdsBind gstorable 
    -- putMsgS "core_bind_types"
    -- putMsg $ ppr $ tts
    -- putMsgS $ "layers " ++ show (length layered)
    -- putMsg $ ppr $ map (map getIdsBind) layered  
    -- putMsgS "errors:"
    -- putMsg $ cat [ppr (length err_msgs), ppr $ isJust m_err]
    
    -- Do other stuff
    (guts', subs, not_subs)<- simplifySubstituteCompile guts layered [] []
    let peeks = filter isPeekId subs
    -- putMsgS $ "gpeeks " ++ show (length peeks)
    -- putMsg  $ ppr $ peeks 
    putMsgS $ "subs: " ++ show (length subs)
    putMsg  $ ppr $ map getIdsBind subs

    -- putMsgS $ "not subs: " ++ show (length not_subs)
    let guts'' = guts' {mg_binds = concat [subs, not_subs, other]}
    return $ guts''
