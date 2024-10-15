{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language NumericUnderscores #-}
{-# language BangPatterns #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Vector.Int64
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
  , foldr
  , ifoldl'
  , ifoldlSlice'
  , traverse_
  , itraverse_
  , replicate
  , construct3
  , construct4
  , append
  , clone
  , cloneSlice
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

import Prelude hiding (replicate,map,maximum,Bounded,all,foldr)

import Vector.Std.Int64
import Vector.Ord.Int64
