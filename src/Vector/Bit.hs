{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language MultiWayIf #-}

module Vector.Bit
  ( -- Types
    Vector(..)
  , Vector#
  , MutableVector(..)
  , MutableVector#
  , Bounded(..)
    -- * Primitives
  , write#
  , write
  , read#
  , index#
  , index
  , unlift
  , substitute
  , initialized
  , unsafeCoerceLength
  , expose
  , expose#
    -- * Ranges
  , set
  , setSlice
    -- * Freeze
  , unsafeShrinkFreeze
  , unsafeFreeze
  , freeze
  , freezeSlice
    -- * Copy
  , thaw
    -- * Composite
  , map
  , ifoldl'
  , ifoldlSlice'
  , construct1
  , construct2
  , construct3
  , construct4
  , construct5
  , replicate
  , append
  , clone
  , cloneSlice
    -- * Index
  , index0
  , index1
  , index2
  , index3
    -- * Equality
  , equals
    -- * Custom
  , zipAnd
  , zipOr
  ) where

import Prelude hiding (replicate, map, Bounded)
import Data.Unlifted (Bool#, pattern True#, pattern False#)

import Vector.Std.Word1
import Vector.Eq.Word1 (equals)
import Arithmetic.Types (Nat#)

import qualified Vector.Zip.Bit.Bit.Bit as Zip

zipOr :: 
     Nat# n
  -> Vector n Bool#
  -> Vector n Bool#
  -> Vector n Bool#
zipOr n xs ys = Zip.zip
  ( \x y ->
      if | False# <- x, False# <- y -> False#
         | otherwise -> True#
  ) n xs ys

zipAnd :: 
     Nat# n
  -> Vector n Bool#
  -> Vector n Bool#
  -> Vector n Bool#
zipAnd n xs ys = Zip.zip
  ( \x y ->
      if | True# <- x, True# <- y -> True#
         | otherwise -> False#
  ) n xs ys
