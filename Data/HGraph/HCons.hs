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
                         -- ** Introduction
                         -- $intro
                         -- ** Safe Constructors
                        mkConsNode
                        , mkLeafNode
                         -- ** Example Usage
                         -- $example
                      ) where

import Data.HGraph.Class (HGraph(..), HGraphApply(..))
import Data.HList.Private

{- $intro

@HGraph@ and @HGraphApply@ instances built on @HList@s
-}

instance HGraph HNil where
    type HP HNil = HNil
    type HN HNil = HNil
    hgPred _ = HNil
    hgNode _ = HNil

instance HList l => HGraph (HCons e l) where
    type HP (HCons e l) = l
    type HN (HCons e l) = e
    hgPred (HCons _ l) = l
    hgNode (HCons e _) = e
                         
instance HApply f l => HGraphApply (HCons (e,f) l) where
    type HGAF (HCons (e,f) l) = f
    hgFun (HCons (_,f) _) = f
    hgApply (HCons (_, f) l) = hApply f l

-- Another instance of HApply for nested HLists -- if we hApply a
-- node's function to its predecessors, we want the application to
-- reach down a level and get the node data of each predecessor.

instance (HList l, HList m, HArg (e -> b) e, HApply b l) => HApply (e -> b) (HCons (HCons e m) l) where
    type HAP (e -> b) (HCons (HCons e m) l) = HAP b l
    hApply f (HCons (HCons e _) l) = hApply (hF f e) l

instance (HList l, HList m, HArg (e -> b) e, HApply b l) => HApply (e -> b) (HCons (HCons (e, g) m) l) where
    type HAP (e -> b) (HCons (HCons (e, g) m) l) = HAP b l
    hApply f (HCons (HCons (e, _) _) l) = hApply (hF f e) l

instance Show e => Show (HCons (e, f) p) where
    show (HCons (e, _) _) = "Node " ++ show e
import Data.HList (hCons, hNil, (+:), HList, (:-), HNil)

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

