{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fforce-recomp -O2 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HApply
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file hgraph/COPYING)
-- 
-- Maintainer  :  peddie@alum.mit.edu
-- Stability   :  experimental
-- Portability :  GHC
--
-- This module has only one class, @HApply@, which allows you to apply
-- a function to a structure of arguments.  It provides instances for
-- a few variations on @HList@s.
-- 
-----------------------------------------------------------------------------

module Data.HApply(
                   -- * Function Application
                   HApply(..)
                   )where

import Data.HList.Private(HArg(..), (:-)(..), HNil, HList)

-- | This class applies a function to a structure of arguments.
class HApply f a where
    -- | The return type of the function once it's been fully applied
    type HAP f a :: *
    -- | @hApply f a@ applies the function @f@ to the argument
    -- structure @a@.
    hApply :: f -> a -> HAP f a

-- Here we choose to return the function @f@: if we @hApply@ a
-- function to an empty argument list, it just means we've got a
-- constant on our hands (or maybe it's a monadic function or
-- something).

instance HApply f HNil where
    type HAP f HNil = f
    hApply f _ = f

instance (HArg (e -> b) e, HApply b l) => HApply (e -> b) (e :- l) where
    type HAP (e -> b) (e :- l) = HAP b l
    hApply f (e :- l) = hApply (hF f e) l

-- Another instance of HApply for nested HLists -- if we hApply a
-- node's function to its predecessors, we want the application to
-- reach down a level and get the node data of each predecessor.

instance (HList l, HList m, HArg (e -> b) e, HApply b l) => HApply (e -> b) ((e :- m) :- l) where
    type HAP (e -> b) ((e :- m) :- l) = HAP b l
    hApply f ((e :- _) :- l) = hApply (hF f e) l

instance (HList l, HList m, HArg (e -> b) e, HApply b l) => HApply (e -> b) (((e, g) :- m) :- l) where
    type HAP (e -> b) (((e, g) :- m) :- l) = HAP b l
    hApply f (((e, _) :- _) :- l) = hApply (hF f e) l


