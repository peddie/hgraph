{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -fforce-recomp -O2 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HList.Private
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file hgraph/COPYING)
-- 
-- Maintainer  :  peddie@alum.mit.edu
-- Stability   :  experimental
-- Portability :  GHC
--
-- An HList implementation on the type level, using type families.
-- This module exports everything, including potentially unsafe
-- constructors.  See Data.HList for better description and the
-- user-friendly interface.
--
-----------------------------------------------------------------------------

module Data.HList.Private(
                          -- * Fundamental Data Types
                          HNil(..)
                         , HCons(..)
                         , HList
                         -- * Basic operations
                         , HHead(..)
                         , HTail(..)
                         , HAppend(..)
                         -- * Higher-Order Operations
                         , HArg(..)
                         , HMap(..)
                         , HApply(..)
                         ) where

import Data.Generics

-- | The empty list
data HNil = HNil deriving (Show, Eq, Data, Typeable)

-- | A cons cell, consisting of a head @e@ and a tail @l@
data HCons e l = HCons e l deriving (Eq, Data, Typeable)

-- | A typeclass for both types of @HList@.  The point is that the
-- elements of an @HList@ don't need to all be the same type.
class HList l
    
instance HList HNil

instance HList l => HList (HCons e l)

-- | A class for taking the head of an @HList@
class HHead e where
    -- | The type of the head of this @HList@
    type HH e l
    -- | Return the head of an @HList@
    hHead :: HH e l -> e

instance HHead e where
    type HH e l = HCons e l
    hHead (HCons e _) = e

-- | A class for taking the tail of an @HList@
class HTail l where
    -- | The type of the tail of this @HList@
    type HT e l
    -- | Return the tail of an @HList@
    hTail :: HT e l -> l

instance HTail l where
    type HT e l = HCons e l
    hTail (HCons _ l) = l

-- | A class for appending two @HList@s together.
class (HList a, HList b) => HAppend a b where
    -- | The type of the resulting @HList@
    type HA a b :: *
    -- | @hAppend a b@ appends @b@ to the end of @a@
    hAppend :: a -> b -> HA a b

instance HAppend HNil HNil where
    type HA HNil HNil = HNil
    hAppend HNil HNil = HNil

instance HList l => HAppend HNil (HCons e l) where
    type HA HNil (HCons e l) = HCons e l
    hAppend HNil h = h

instance HList l => HAppend (HCons e l) HNil where
    type HA (HCons e l) HNil = HCons e l
    hAppend h HNil = h

instance (HList l, HList m, HAppend l (HCons f m)) => HAppend (HCons e l) (HCons f m) where
    type HA (HCons e l) (HCons f m) = HCons e (HA l (HCons f m))
    hAppend (HCons e l) (HCons f m) = HCons e (hAppend l (HCons f m))

-- | This is a utility class for explaining to the typechecker that a
-- function is known to be applicable to a particular argument.
class HArg f a where
    -- | The type that results from the function application
    type HAr f a
    -- | Apply a function to an argument
    hF :: f -> a -> HAr f a

-- | We add an instance for functions in general, and that pretty much
-- takes care of things.
instance HArg (a -> b) a where
    type HAr (a -> b) a = b
    hF f = f

-- | A class for mapping a function over an @HList@
class HList a => HMap f a where
    -- | The type of the resulting @HList@
    type HM f a
    -- | @hMap f a@ applies the function @f@ to every element of the
    -- @HList@ @a@
    hMap :: f -> a -> HM f a

instance HMap f HNil where
    type HM f HNil = HNil
    hMap _ HNil = HNil

instance (HList l, HArg f e, HMap f l) => HMap f (HCons e l) where
    type HM f (HCons e l) = HCons (HAr f e) (HM f l)
    hMap f (HCons e l) = HCons (hF f e) (hMap f l)

-- | This class is the crown jewel of the library; it applies a
-- function to a heterogeneous @HList@ of arguments.
class HList a => HApply f a where
    type HAP f a :: *
    hApply :: f -> a -> HAP f a

-- Here we choose to return the function @f@: if we @hApply@ a
-- function to an empty argument list, it just means we've got a
-- constant on our hands (or maybe it's a monadic function or
-- something).
instance HApply f HNil where
    type HAP f HNil = f
    hApply f _ = f

instance (HList l, HArg (e -> b) e, HApply b l) => HApply (e -> b) (HCons e l) where
    type HAP (e -> b) (HCons e l) = HAP b l
    hApply f (HCons e l) = hApply (hF f e) l
