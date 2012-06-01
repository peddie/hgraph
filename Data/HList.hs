{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fforce-recomp -O2 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HList
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file hgraph/COPYING)
-- 
-- Maintainer  :  peddie@alum.mit.edu
-- Stability   :  experimental
-- Portability :  GHC
--
-- An @HList@ implementation on the type level, using type families.
-- This module re-exports the safe parts of Data.HListPrivate, along
-- with some safe constructors. The idea behind @HList@ is that you
-- can define heterogenous lists and enjoy the same list functions
-- (head, append, map, etc.) that you're used to wherever they are
-- typesafe (e.g. when the function you want to @map@ really does
-- apply to all the elements).  This involves @TypeFamilies@,
-- @MultiParamTypeClasses@, @FlexibleInstances@ and @FlexibleContexts@
-- to get the typechecker to accept it; it does _not_ require
-- @UndecideableInstances@.
-- 
-- This is a partial reimplementation of Oleg's @HList@ library, but
-- he used @FunctionalDependencies@ and @UndecideableInstances@ to
-- make the typechecker happy.  Not all the functions are
-- reimplemented due to time constraints.
--
-----------------------------------------------------------------------------

module Data.HList(
                  -- * Safe Constructors
                  hCons
                 , hNil
                  -- * Syntactic Sugar
                 , (:-)
                 , (-:)
                 -- * Fundamental Data Types
                 , HList
                 -- * Basic Operations
                 , HHead(..)
                 , HTail(..)
                 -- $headtail
                 , HAppend(..)
                 -- * Higher-Order Operations
                 , HArg(..)
                 , HMap(..)
                 , HApply(..)
                  -- * Example Usage
                  -- $example
                 ) where

import Data.HList.Private

{- $headtail

   Note that the @hHead@ and @hTail@ functions can only be called on
   an @HCons@!
-}

-- Export safe constructors for HNil and HCons to ensure that the
-- user can't cons onto something other than an HList.

-- | @hNil@ returns an empty @HList@
hNil :: HNil
hNil = HNil

-- | @hCons e l@ conses a new element @e@ onto the @HList@ @l@
hCons :: HList l => e -> l -> HCons e l
hCons = HCons

-- | @:-@ is a type synonym for @HCons@: @e :- l@ is the same as
-- @HCons e l@
type e :- l = HCons e l

infixr 9 :-

-- | @-:@ is an infix operator: @e -: l@ is the same as @hCons e l@
(-:) :: HList l => e -> l -> HCons e l
(-:) = hCons

infixr 9 -:

{- $example

> testa = 22 -: hNil
> testb = "asdf" -: testa
> 
>   :type testb
>     testb :: HCons [Char] (HCons Integer HNil)

-}
