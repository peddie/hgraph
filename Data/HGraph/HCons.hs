{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fforce-recomp -O2 #-}
{-# OPTIONS_HADDOCK prune #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HGraph.HCons
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file hgraph/COPYING)
-- 
-- Maintainer  :  peddie@alum.mit.edu
-- Stability   :  experimental
-- Portability :  GHC
--
-- This module gives an example instance of HGraph directly on
-- @HLists@
--
-----------------------------------------------------------------------------

module Data.HGraph.HCons(
                         -- * Safe Constructors
                        mkConsNode
                        , mkLeafNode
                         -- * Example Usage
                         -- $example
                      ) where

import Data.HGraph ()
import Data.HList (hCons, hNil, (+:), HList, (:-), HNil)
import Data.HApply

-- | @mkConsNode a f p@ creates a node in an HGraph with data @a@,
-- function @f@ and @HList@ of predecessors @p@.
mkConsNode :: (HList p, HApply f p) => a -> f -> p -> (a, f) :- p
mkConsNode d f = hCons (d, f)

-- | @mkLeafNode a@ creates a node in an HGraph with data @a@
mkLeafNode :: a -> (a, b -> b) :- HNil
mkLeafNode d = (d, id) +: hNil

{- $example

> f :: Integer -> String -> String
> f n s = show n ++ " " ++ s
> 
> testa = mkLeafNode (22 :: Integer)
> testb = mkLeafNode "22"
> testc = mkConsNode "Twenty-Two" f (testa +: testb +: hNil)
> 
>   hgApply testc
>     "22 22"

-}

