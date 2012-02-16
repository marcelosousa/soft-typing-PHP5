{-# LANGUAGE TypeOperators, TypeSynonymInstances #-}
module MF.Language.PHP.Types where

import Data.Set as S 
import Data.Map as M
import Data.List as L
import Data.Maybe

import qualified Debug.Trace as T

import MF.Core.Flowable   
import MF.Core.Solver
import MF.Core.Context
import MF.Core.Lattice


--trace' = T.trace
trace' _ = id

-------------------------------------------------------------------------------
-- TypeSet 
-------------------------------------------------------------------------------

type TypeSet = Set TypeUniverse

data TypeUniverse = Scalar Scalar
                  | Compound Compound
                  | TyRef (Identifier, TypeUniverse)
                  | TyAny
                  | Null
            deriving (Eq, Ord, Show, Read)                            
            
data Scalar = TyInt
            | TyFloat
            | TyBool
            | TyString
            deriving (Eq, Ord, Show, Read)                            
            
data Compound = TyArray TypeUniverse
             -- | TyObject 
             -- | TyResource
             deriving (Eq, Ord, Show, Read)                            

class Type t where
    toArray   :: t -> t
    fromArray :: t -> t
    depth     :: t -> Int

instance Type TypeUniverse where
    toArray                           = Compound . TyArray
    fromArray (Compound (TyArray t))  = t
    fromArray _                       = error "Expecting TyArray"    
    depth (Compound (TyArray t))      = 1 + depth t
    depth _                           = 0

instance (Ord t, Type t) => Type (Set t) where
    toArray                = S.map toArray
    fromArray              = S.map fromArray
    depth set | S.null set = 0
              | otherwise  = S.findMax . S.map depth $ set


fromArrayRepeatedly :: (Type t) => Int -> t -> t
fromArrayRepeatedly n t = foldr ($) t $ take n $ repeat fromArray

toArrayRepeatedly :: (Type t) => Int -> t -> t
toArrayRepeatedly n t = foldr ($) t $ take n $ repeat toArray


instance Lattice TypeSet where
    -- This is different than the original widening functions. 
    -- 1. We take the maximum value when we extend the depth function to sets
    -- 2. We always return a array type of depth k when the depth of l `union` r exceeds k. 
    join l r | depth' < k = trace' ("Joining " ++ show l ++ " with " ++ show r)  $ l `S.union` r
             | otherwise  = trace' ("Failback") $ S.singleton $ toArrayRepeatedly k TyAny
                          where
                              depth' = depth $ l `S.union` r
                              k      = 3
    leftjoin = join
    rightjoin = join
    (<:) = S.isSubsetOf




-------------------------------------------------------------------------------
-- Constraints
-------------------------------------------------------------------------------

data Constraint = Label :<=: Label          -- Dependency
                | Label :==: TypeSet
                deriving (Eq, Ord, Show)

isEquality :: Constraint -> Bool
isEquality (l :==: t) = True
isEquality _          = False

hasLabel :: Label -> Constraint -> Bool
hasLabel label (l :==: t) = l == label
hasLabel label _          = False


resolve :: Set Constraint -> Set Constraint
resolve constraints = trace' ("Resolving " ++ show constraints ++ " produces " ++ show result) $ result
    where
        result = S.fold resolve' S.empty constraints
        resolve' c@(l1 :<=: l2) r = S.insert c r `S.union` S.fold update S.empty constraints
                                  where
                                      update (l2' :==: t) r | l2 == l2' = S.insert (l1 :==: t) r
                                                            | otherwise = r
                                      update c                        r = r
        resolve' c            r = S.insert c r
        
fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f a | a == na   = a
             | otherwise = fixPoint f na
                         where na = f a
                         
resolveType :: Set Constraint -> Label -> TypeSet
resolveType constraints label = trace' ("resolving type for: " ++ show label ++ " with " ++ show constraints) $ 
  S.fold join S.empty . S.map toType . S.filter (hasLabel label) . fixPoint resolve $ constraints
    where        
        toType (l :==: t) = t

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

data Identifier = Identifier String
                | Parameter (Int,String,String,Bool)
                | ReturnValue
                deriving (Ord,Eq,Show,Read)

type Mapping = Identifier :-> TypeSet

instance Lattice Mapping where
    join = M.unionWith join
    leftjoin = M.unionWith const
    rightjoin = M.union
    (<:) = M.isSubmapOfBy (<:)

instance (Ord c) => Lattice (c :-> Mapping) where
    join = M.unionWith join
    leftjoin = M.unionWith leftjoin
    rightjoin = M.unionWith rightjoin
    (<:) = M.isSubmapOfBy (<:)

createRef :: Identifier -> TypeSet -> TypeSet
createRef vname setTy = S.map (\ty -> TyRef (vname, ty)) setTy


updateReferences :: Identifier -> Mapping -> TypeSet -> Mapping
updateReferences vname mapping effect = M.map (\tyset -> updateTypeSet vname tyset effect) mapping

updateTypeSet :: Identifier -> TypeSet -> TypeSet -> TypeSet
updateTypeSet vname tyset effect = let tylist = S.toList tyset
                                   in if any (hasRef vname) tylist
                                    then let tylist' = L.filter (\ty -> not (hasRef vname ty)) tylist
                                         in addRefTypes vname effect (S.fromList tylist')
                                    else tyset 

hasRef :: Identifier -> TypeUniverse -> Bool
hasRef vname (TyRef (vname',_)) = vname == vname'
hasRef _     _                      = False

addRefTypes :: Identifier -> TypeSet -> TypeSet -> TypeSet
addRefTypes vname effect tyset = S.union tyset (S.map (\ty -> TyRef (vname, ty)) effect)

changeTypeRef :: TypeUniverse -> TypeSet -> TypeSet
changeTypeRef ty s = S.map (\t -> changeTypeRef' ty t) s

changeTypeRef' :: TypeUniverse -> TypeUniverse -> TypeUniverse
changeTypeRef' (TyRef (n, _)) ty = TyRef (n,ty)
changeTypeRef' _              ty = ty

-- Marcelo Sousa version
-- We resolve the constraints 
-- Filter equality constraints and lift there type in case of any arrays
-- Create possible references
-- Update values of references
updateMapping :: Identifier -> Maybe Identifier -> Label -> Int -> Bool -> Set Constraint -> Mapping -> Mapping
updateMapping identifier v label depth isRef constraints mapping =
      let types = S.map (\(l :==: t) -> toArrayRepeatedly depth t) . S.filter isApplicable      
          effect = S.fold join S.empty . types . (fixPoint resolve) $ constraints
      in trace' ("Processing " ++ (show identifier) ++ "(" ++ show label ++ ") with " ++ show constraints ++ " resulting in: " ++ show effect) $ 
         if isRef
          then let rhs      = fromJust v
                   effect'  = createRef identifier effect
                   effect'' = createRef rhs        effect
               in M.insert identifier effect'' $ M.insert rhs effect' mapping
          else let mapping' = updateReferences identifier mapping effect
               in case M.lookup identifier mapping' of
                          Just t -> let effect' = S.fold (\ty r -> S.union (changeTypeRef ty effect) r) S.empty t
                                    in M.insert identifier effect' mapping'
                          Nothing -> M.insert identifier effect mapping'
    where        
        isApplicable (l :==: t) = l == label
        isApplicable _          = False
        
-- Previous version
{-
updateMapping :: Identifier -> Label -> Int -> Bool -> Set Constraint -> Mapping -> Mapping
updateMapping identifier label depth constraints isRef mapping = 
      let context = S.empty --case M.lookup identifier mapping of
                    Just t  -> t
                    Nothing -> S.empty
          effect =  S.fold join context . types . (fixPoint resolve) $ constraints
          types = S.map (\(l :==: t) -> toArrayRepeatedly depth t) . S.filter isApplicable      
      in trace' ("Processing " ++ (show identifier) ++ "(" ++ show label ++ ") with " ++ show constraints ++ " resulting in: " ++ show effect) $ 
         M.insert identifier effect mapping
    where        
        -- We add to what we already know
        -- We resolve the constraints and join the result with our context value
        -- Filter equality constraints and lift there type in case of any arrays
        isApplicable (l :==: t) = l == label
        isApplicable _          = False
-}                              