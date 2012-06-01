{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fforce-recomp -O2 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HGraph.Class
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file hgraph/COPYING)
-- 
-- Maintainer  :  peddie@alum.mit.edu
-- Stability   :  experimental
-- Portability :  GHC
--
-- Directed graphs on the type level -- fundamental classes
--
-----------------------------------------------------------------------------

module Data.HGraph.Class(
                         -- ** Generic graphs
                         HGraph(..)
                         -- ** Generic graphs with computations on predecessors
                        , HGraphApply(..)
                        ) where

import Data.HList (HApply(..))

-- | A generic class for directed acyclic graphs, with arbitrary data
-- @HN a@ and a set of predecessors associated with each node
class HGraph a where
    -- | The type of this graph instance's predecessors
    type HP a
    -- | The type of this node's data
    type HN a
    -- | Get the predecessors of this node
    hgPred :: a -> HP a
    -- | Get the node data of this node
    hgNode :: a -> HN a

-- | A class for graphs where each node is endowed with a function
-- whose arguments are, in order, the node data of its predecessors.
-- HApply is used to link the function to the predecessors, but
-- there's still no restriction on what the node itself looks like.
class HGraph a => HGraphApply a where
    -- | The output type of this graph's function
    type HGAF a
    -- | Get the function of this node
    hgFun :: a -> HGAF a
    -- | Apply this node's function to its predecessors
    hgApply :: HApply (HGAF a) (HP a) => a -> HAP (HGAF a) (HP a)
    hgApply node = hApply (hgFun node) (hgPred node)

