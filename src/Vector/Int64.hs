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
  , freeze#
  , freezeSlice#
    -- * Copy
  , thaw
    -- * Composite
  , map
  , traverse_
  , itraverse_
  , itraverse_#
  , traverseST#
  , foldr
  , ifoldl'
  , ifoldlSlice'
  , replicate
  , empty
  , empty#
  , empty_
  , construct1
  , construct2
  , construct3
  , construct4
  , construct5
  , construct6
  , construct7
  , construct1#
  , construct2#
  , construct3#
  , construct4#
  , construct7#
  , construct1_
  , construct2_
  , construct3_
  , construct4_
  , construct7_
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
  , elem
  , findIndexEq
  , maximum
  , maximumSlice
  , maximumSliceInitial
  , minimum
  , minimumSlice
  , minimumSliceInitial
  , bubbleSort
  , bubbleSortSlice
  , bubbleSortSliceInPlace
  , mapEq
    -- * Show
  , show
    -- * Interop with primitive
  , cloneFromByteArray
  ) where

import Prelude hiding (replicate,map,maximum,Bounded,all,foldr,show,minimum,elem)

import Arithmetic.Types (Nat#)
import Data.Primitive (ByteArray)
import GHC.Exts (Int64#)
import GHC.Int (Int64(I64#))

import Vector.Eq.Int64
import Vector.Std.Int64
import Vector.Ord.Int64

import qualified Vector.Prim.Int64

-- | Crashes the program if the range is out of bounds. That is,
-- behavior is always well defined.
--
-- Interprets the bytes in a native-endian fashion.
cloneFromByteArray ::
     Int    -- ^ Offset into byte array, units are elements, not bytes
  -> Nat# n -- ^ Length of the vector, units are elements, not bytes
  -> ByteArray
  -> Vector n Int64#
cloneFromByteArray = Vector.Prim.Int64.unsafeCloneFromByteArray

show :: Nat# n -> Vector n Int64# -> String
show n v = liftShows (\i s -> shows (I64# i) s) n v ""
