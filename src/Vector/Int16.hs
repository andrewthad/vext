{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language NumericUnderscores #-}
{-# language BangPatterns #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Vector.Int16
  ( -- Types
    Vector(..)
  , Vector#
  , MutableVector(..)
  , MutableVector#
  , Bounded(..)
  , Vector_(..)
  , FromMutability#
    -- * Primitives
  , write#
  , write
  , read#
  , index#
  , index
  , unlift
  , substitute
  , substitute#
  , initialized
  , initialized#
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
  , traverse_
  , itraverse_
  , itraverse_#
  , traverseST#
  , ifoldl'
  , ifoldlSlice'
  , replicate
  , empty
  , empty_
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
  , findIndexEq
  , maximum
  , maximumSlice
  , maximumSliceInitial
  , bubbleSort
  , bubbleSortSlice
  , bubbleSortSliceInPlace
  , mapEq
  ) where

import Prelude hiding (replicate,map,maximum,Bounded,all)

import Vector.Std.Int16
import Vector.Ord.Int16
import Vector.Eq.Int16
