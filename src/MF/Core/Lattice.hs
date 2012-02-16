{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module MF.Core.Lattice where
    
import Data.Map as M

type c :-> l = M.Map c l

class Lattice a where
    join   :: a -> a -> a
    leftjoin :: a -> a -> a
    rightjoin :: a -> a -> a
    (<:)   :: a -> a -> Bool
