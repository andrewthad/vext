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
  , any
  , all
  , map
  , traverse_
  , traverseZip_
  , traverseST#
  , itraverse_
  , foldlM
  , ifoldl'
  , ifoldlSlice'
  , ifoldr
  , foldr
  , foldrZip
  , replicate
  , empty
  , empty_
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
    -- * Show
  , show
    -- * Interop with primitive
  , cloneFromByteArray
  ) where

import Prelude hiding (replicate,map,maximum,Bounded,all,any,elem,show,foldr)

import Arithmetic.Types (Nat#)
import Data.Primitive (ByteArray)
import GHC.Exts (Word8#)
import GHC.Word (Word8(W8#))

import Vector.Std.Word8
import Vector.Ord.Word8
import Vector.Eq.Word8

import qualified Vector.Prim.Word8

-- | Crashes the program if the range is out of bounds. That is,
-- behavior is always well defined.
--
-- Interprets the bytes in a native-endian fashion.
cloneFromByteArray ::
     Int    -- ^ Offset into byte array, units are elements, not bytes
  -> Nat# n -- ^ Length of the vector, units are elements, not bytes
  -> ByteArray
  -> Vector n Word8#
cloneFromByteArray = Vector.Prim.Word8.unsafeCloneFromByteArray

show :: Nat# n -> Vector n Word8# -> String
show n v = liftShows (\i s -> shows (W8# i) s) n v ""
