{-# language MagicHash #-}

module Vector.Int32
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
    -- * Ranges
  , set
    -- * Freeze
  , unsafeShrinkFreeze
  , unsafeFreeze
    -- * Copy
  , thaw
    -- * Composite
  , map
  , ifoldl'
  , ifoldlSlice'
  , replicate
  , construct3
  , construct4
  , append
  , clone
  , cloneSlice
    -- * Index
  , index0
  , index1
  , index2
  , index3
    -- * Ordered
  , unique
  , equals
  , maximum
  , maximumSlice
  , maximumSliceInitial
  , bubbleSort
  , bubbleSortSlice
  , bubbleSortSliceInPlace
  , mapEq
  ) where

import Prelude ()

import Vector.Std.Int32
import Vector.Ord.Int32
