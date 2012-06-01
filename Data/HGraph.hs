{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall -fforce-recomp -O2 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HGraphType
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file hgraph/COPYING)
-- 
-- Maintainer  :  peddie@alum.mit.edu
-- Stability   :  experimental
-- Portability :  GHC
--
-- Directed graphs on the type level
--
-----------------------------------------------------------------------------

module HGraph where

import "hgraph" Data.HList

data HGraph' a where
    Node :: a -> HGraph' a
    FNode :: HApply f p => a -> f -> p -> HGraph' a

-- | A class for graphs

class HGraph a where
    type HP a
    type HN a
    hgPred :: a -> HP a
    hgNode :: a -> HN a

-- | A glass for graphs where each node is endowed with a function
-- whose arguments are, in order, the node data of its predecessors.

class HGraph a => HGraphApply a where
    type HGAF a
    hgFun :: a -> HGAF a
    hgApply :: HApply (HGAF a) (HP a) => a -> HAP (HGAF a) (HP a)
    hgApply node = hApply (hgFun node) (hgPred node)

-- | HGraph and HGraphApply instances built on HLists

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
    hgApply (HCons (e, f) l) = hApply f l

-- | Another instance of HApply for nested HLists -- if we hApply a
-- node's function to its predecessors, we want the application to
-- reach down a level and get the node data of each predecessor.

instance (HList l, HList m, HArg (e -> b) e, HApply b l) => HApply (e -> b) (HCons (HCons e m) l) where
    type HAP (e -> b) (HCons (HCons e m) l) = HAP b l
    hApply f (HCons (HCons e m) l) = hApply (hF f e) l

instance (HList l, HList m, HArg (e -> b) e, HApply b l) => HApply (e -> b) (HCons (HCons (e, g) m) l) where
    type HAP (e -> b) (HCons (HCons (e, g) m) l) = HAP b l
    hApply f (HCons (HCons (e, g) m) l) = hApply (hF f e) l

instance Show e => Show (HCons (e, f) p) where
    show (HCons (e, _) _) = "Node " ++ show e

mkConsNode :: HApply f p => a -> f -> p -> HCons (a, f) p
mkConsNode d f = HCons (d, f)

-- mkNode :: (HGraph a, HGraphApply a) => 
-- mkNode d f p = 

mkNode :: HList l => a -> l -> HCons a l
mkNode = hCons

testa = mkNode 22 HNil
testb = mkNode "22" HNil
testd = mkNode ("Twenty-Two", testf) (HCons testa (HCons testb HNil))

