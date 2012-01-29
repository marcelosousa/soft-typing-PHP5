{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module MF.Core.Lattice where
    
import Data.Map as M

type c :-> l = M.Map c l

class Lattice a where
    join   :: a -> a -> a
    (<:)   :: a -> a -> Bool

instance (Lattice l, Ord c) => Lattice (c :-> l) where
    join = M.unionWith join
    (<:) = M.isSubmapOfBy (<:)
