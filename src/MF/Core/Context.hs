{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-------------------------------------------------------------------------------
-- Module    :  MF.Core.Context
-- Copyright :  (c) 2012 Marcelo Sousa, Henk Erik van der Hoek
-------------------------------------------------------------------------------

module MF.Core.Context where
    
import Data.Map as M
import Data.List as L

import MF.Core.Flowable
import MF.Core.Lattice

type Stack = [Int]

class Context a where
    lift :: (Lattice l, Lattice (a :-> l)) => (Block node -> l -> l) -> Block node -> (a :-> l) -> (a :-> l)
    
instance Context Label where
    lift transfer stmt = M.map (transfer stmt)

instance Context Stack where
    lift transfer stmt@(Entry        _) = M.map (transfer stmt)
    lift transfer stmt@(Exit         _) = M.map (transfer stmt)               
    lift transfer stmt@(Call   lc lr _) = M.map (transfer stmt) . M.mapKeysWith join (take 3 . (:) lc) 
    lift transfer stmt@(Return lc lr _) = M.map (transfer stmt) . M.mapKeys tail . M.filterWithKey (\k v -> not (L.null k) && head k == lc)
    lift transfer stmt@(Normal       _) = M.map (transfer stmt)
    lift transfer stmt@(Before       _) = M.map (transfer stmt)
    lift transfer stmt@(After        _) = M.map (transfer stmt)
