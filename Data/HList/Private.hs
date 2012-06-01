{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
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
                         , (:-)(..)
                         , HCons
                         , HList
                         -- * Basic operations
                         , HHead(..)
                         , HTail(..)
                         , HAppend(..)
                         -- * Higher-Order Operations
                         , HArg(..)
                         , HMap(..)
                         ) where

import Data.Generics

-- | The empty list
data HNil = HNil deriving (Show, Eq, Data, Typeable)

-- | A cons cell, consisting of a head @e@ and a tail @l@
data e :- l = e :- l deriving (Show, Eq, Data, Typeable)

-- Some sugar if you like

infixr 9 :-
type HCons e l = e :- l

-- | A typeclass for both types of @HList@.  The point is that the
-- elements of an @HList@ don't need to all be the same type.
class HList l
    
instance HList HNil

instance HList l => HList (e :- l)

-- | A class for taking the head of an @HList@
class HHead e where
    -- | The type of the head of this @HList@
    type HH e l
    -- | Return the head of an @HList@
    hHead :: HH e l -> e

instance HHead e where
    type HH e l = e :- l
    hHead (e :- _) = e

-- | A class for taking the tail of an @HList@
class HTail l where
    -- | The type of the tail of this @HList@
    type HT e l
    -- | Return the tail of an @HList@
    hTail :: HT e l -> l

instance HTail l where
    type HT e l = e :- l
    hTail (_ :- l) = l

-- | A class for appending two @HList@s together.
class (HList a, HList b) => HAppend a b where
    -- | The type of the resulting @HList@
    type HA a b :: *
    -- | @hAppend a b@ appends @b@ to the end of @a@
    hAppend :: a -> b -> HA a b

instance HAppend HNil HNil where
    type HA HNil HNil = HNil
    hAppend HNil HNil = HNil

instance HList l => HAppend HNil (e :- l) where
    type HA HNil (e :- l) = e :- l
    hAppend HNil h = h

instance HList l => HAppend (e :- l) HNil where
    type HA (e :- l) HNil = e :- l
    hAppend h HNil = h

instance (HList l, HList m, HAppend l (f :- m)) => HAppend (e :- l) (f :- m) where
    type HA (e :- l) (f :- m) = e :- HA l (f :- m)
    hAppend (e :- l) (f :- m) = e :- hAppend l (f :- m)

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

instance (HList l, HArg f e, HMap f l) => HMap f (e :- l) where
    type HM f (e :- l) = HAr f e :- HM f l
    hMap f (e :- l) = hF f e :- hMap f l

