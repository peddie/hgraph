{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fforce-recomp -O2 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HGraph.Graph
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file hgraph/COPYING)
-- 
-- Maintainer  :  peddie@alum.mit.edu
-- Stability   :  experimental
-- Portability :  GHC
--
-- This module gives an example instance of Data.Graph using a new
-- data type.
--
-----------------------------------------------------------------------------

module Data.HGraph.Graph(
                         -- * Safe Constructors
                         mkNode
                        , mkLeaf
                         -- * Example Usage
                         -- $example
                        ) where

import Data.Generics

import Data.HGraph.Class
import Data.HApply
import Data.HList.Private
import Data.HList

-- | A node in a graph
data HNode a f p = HNode a f p deriving (Data, Typeable)

instance Eq a => Eq (HNode a f p) where
    (HNode a _ _) == (HNode b _ _) = a == b

instance Show a => Show (HNode a f p) where
    show (HNode a _ _) = "HNode <" ++ show a ++ ">"

instance HGraph (HNode a f p) where
    type HP (HNode a f p) = p
    type HN (HNode a f p) = a
    hgPred (HNode _ _ p) = p
    hgNode (HNode a _ _) = a

instance HGraphApply (HNode a f p) where
    type HGAF (HNode a f p) = f
    hgFun (HNode _ f _) = f

instance (HList l, HApply b l) => HApply (a -> b) (HNode a g q :- l) where
    type HAP (a -> b) (HNode a g q :- l) = HAP b l
    hApply f (HNode a _ _ :- l) = hApply (hF f a) l

-- | @mkNode a f p@ creates a new node from a node value @a@, a
-- function @f@ and an @HList@ of predecessors @p@.
mkNode :: HApply f p => a -> f -> p -> HNode a f p
mkNode = HNode

-- | @mkLeaf a@ creates a new node with only a value @a@
mkLeaf :: a -> HNode a (b -> b) HNil
mkLeaf x = HNode x id hNil


{- $example 

> g :: Int -> String -> String
> g n s = show n ++ s
> 
> testa = mkLeaf (22 :: Int)
> testb = mkLeaf ("22" :: String)
> testc = mkNode "Twenty-Two" g (testa -: testb -: hNil)
> 
> hgApply testc
>   "2222"

-}
