{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language NumericUnderscores #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Vector.Word64
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
  , traverseZip_
  , traverseST#
  , ifoldl'
  , ifoldlSlice'
  , replicate
  , empty
  , empty#
  , empty_
  , construct3
  , construct4
  , append
  , clone
  , cloneSlice
  , sum
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
  , bubbleSort
  , bubbleSortSlice
  , bubbleSortSliceInPlace
  , mapEq
    -- * Show
  , show
    -- * Interop with primitive
  , cloneFromByteArray
  , toPrimArray
  ) where

import Prelude hiding (replicate,map,maximum,Bounded,all,elem,show,sum)

import Arithmetic.Types (Nat#)
import Data.Primitive (ByteArray)
import GHC.Exts (Word64#)
import GHC.Word (Word64(W64#))

import Vector.Std.Word64
import Vector.Eq.Word64
import Vector.Ord.Word64
import Data.Primitive (PrimArray(PrimArray))
import Data.Unlifted (PrimArray#(PrimArray#))

import qualified GHC.Exts as Exts
import qualified Vector.Prim.Word64

-- | Crashes the program if the range is out of bounds. That is,
-- behavior is always well defined.
--
-- Interprets the bytes in a native-endian fashion.
cloneFromByteArray ::
     Int    -- ^ Offset into byte array, units are elements, not bytes
  -> Nat# n -- ^ Length of the vector, units are elements, not bytes
  -> ByteArray
  -> Vector n Word64#
{-# inline cloneFromByteArray #-}
cloneFromByteArray = Vector.Prim.Word64.unsafeCloneFromByteArray

toPrimArray :: Vector n Word64# -> PrimArray Word64
{-# inline toPrimArray #-}
toPrimArray v = case expose v of
  PrimArray# x -> PrimArray x

show :: Nat# n -> Vector n Word64# -> String
show n v = liftShows (\i s -> shows (W64# i) s) n v ""

sum :: Nat# n -> Vector n Word64# -> Word64#
{-# noinline sum #-}
sum n !v =
  let !(W64# result) = ifoldl'
        (\(W64# acc) _ (x :: Word64#) -> W64# (Exts.plusWord64# acc x))
        (W64# (Exts.wordToWord64# 0##))
        n
        v
   in result
