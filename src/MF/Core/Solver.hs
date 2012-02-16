module MF.Core.Solver where

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.IntSet as IS
import qualified Data.Set    as S
import           Data.Maybe
import           Prelude hiding (lookup, init, all)

import           MF.Core.Flowable
import           MF.Core.Context
import           MF.Core.Lattice

import qualified Debug.Trace as T 

--trace = T.trace
trace _ = id

data Direction = Forward | Backward
type ValueMap property = IM.IntMap property

-- | Helper function to lookup a value in a ValueMap when given a label
lookup :: Label -> ValueMap a -> a
lookup l m = case IM.lookup l m of 
                 Just a  -> a
                 Nothing -> error $ "Looking for non existing label: " ++ show l ++ "in " ++ show (IM.keys m)
                 
-- | Helper function to reverse the flow in case of a backwards analysis
reverseFlow :: Flow -> Flow
reverseFlow = map (\(l, l') -> (l', l))


mergeWith :: (a -> b -> c) -> ValueMap a -> ValueMap b -> ValueMap c
mergeWith f left = IM.mapWithKey (\k -> f $ func $ IM.lookup k left)
    where
        func a = case a of 
                     Just t  -> t
                     Nothing -> error "Couldn't merge"

-- | Helper function to apply the transfer function to all values in a ValueMap
all :: (Flowable node, Lattice l) => (Block node -> l -> l) -> node -> ValueMap l -> ValueMap l
all transfer program = mergeWith transfer (blocks program)


finalFlowOut :: FlowOut
finalFlowOut = ((-1,-1),[],[])

-- | 'solve' implements the worklist algorithm to compute the MFP solution as described by NNH, page 75
solve :: (Flowable n, Lattice l, Show l) => (Block n -> l -> l) -> l -> l -> Direction -> n -> ValueMap (ValueMap l, FlowOut)
solve transfer extremalValue bottom direction p = solve' 1 IM.empty p initialValueMap worklist
    where                
        -- Step 1. Initialization
        worklist       = case direction of 
                            Forward  -> flow p -- trace ("solve " ++ show (flow p)) $ flow p
                            Backward -> reverseFlow . flow $ p
                
        extremalLabels = case direction of 
                            Forward  -> [init p]
                            Backward -> final p
                
        initialValueMap = IM.fromList $ map initialize $ labels p
            where
                initialize l = if l `elem` extremalLabels 
                                  then (l, extremalValue) 
                                  else (l, bottom)
                
        -- Step 2. Fix point iteration
        solve' n it p valueMap []                            = IM.insert n ((all transfer p valueMap), finalFlowOut) it -- Step 3. From context to effect values
        solve' n it p valueMap w@((start, end):worklistTail) = trace ("solving worklist" ++ show (start,end)) $ 
          let context  = lookup start valueMap
              previous = lookup end valueMap                             
              effect = transfer (lookup start $ blocks p) context
              
              newWorklist = worklistTail ++ [(l', l'') | (l', l'') <- worklist, l' == end] 
              newValueMap = case lookup start $ blocks p of
                                 Return _ _ _ -> case lookup end $ blocks p of
                                                      After _ -> let k = IM.adjust (leftjoin effect) end valueMap
                                                                 in trace ("Leftjoin of " ++ show effect ++ " \nbefore = " ++ show previous ++ "\nafer = " ++ show (lookup end k)) $ k
                                                      _       -> IM.adjust (join effect) end valueMap
                                 Before _     -> case lookup end $ blocks p of
                                                      After _ -> let k = IM.adjust (\l -> rightjoin l effect) end valueMap
                                                                 in trace ("Rightjoin of " ++ show effect ++ " \nbefore = " ++ show previous ++ "\nafer = " ++ show (lookup end k)) $ k
                                                      _       -> IM.adjust (join effect) end valueMap
                                 After _ -> let k = IM.adjust (const effect) end valueMap
                                            in trace ("After join of " ++ show effect ++ " \nbefore = " ++ show previous ++ "\nafer = " ++ show (lookup end k)) $ k
                                 _       -> IM.adjust (join effect) end valueMap
              foutt = ((start,end), w, worklistTail)
              foute = ((start,end), w, newWorklist)
          in if (effect <: previous)
             then solve' (n+1) (IM.insert n (valueMap, foutt) it) p valueMap worklistTail
             else solve' (n+1) (IM.insert n (newValueMap, foute) it) p newValueMap newWorklist                                  
                