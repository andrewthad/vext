{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language NumericUnderscores #-}
{-# language BangPatterns #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Vector.Word8
  ( -- Types
    Vector(..)
  , Vector#
  , MutableVector(..)
  , MutableVector#
  , Bounded(..)
  , Vector_(..)
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
  , any
  , all
  , map
  , ifoldl'
  , ifoldlSlice'
  , replicate
  , construct1
  , construct3
  , construct4
  , construct5
  , construct6
  , construct7
  , append
  , clone
  , cloneSlice
    -- * Index
  , index0
  , index1
  , index2
  , index3
  , index4
  , index5
  , index6
  , index7
  , index8
    -- * Ordered
  , unique
  , equals
  , elem
  , findIndexEq
  , maximum
  , maximumSlice
  , maximumSliceInitial
  , bubbleSort
  , bubbleSortSlice
  , bubbleSortSliceInPlace
  , mapEq
    -- * Hide Length
  , vector_
  ) where

import Prelude hiding (replicate,map,maximum,Bounded,all,any,elem)

import Vector.Std.Word8
import Vector.Ord.Word8
import Vector.Eq.Word8

