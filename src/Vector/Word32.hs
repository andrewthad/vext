{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language NumericUnderscores #-}
{-# language BangPatterns #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Vector.Word32
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
  ) where

import Prelude hiding (replicate,map,maximum,Bounded,all,any,elem,show)

import Arithmetic.Types (Nat#)
import Data.Primitive (ByteArray)
import GHC.Exts (Word32#)
import GHC.Word (Word32(W32#))

import Vector.Std.Word32
import Vector.Eq.Word32
import Vector.Ord.Word32

import qualified Vector.Prim.Word32

-- | Crashes the program if the range is out of bounds. That is,
-- behavior is always well defined.
--
-- Interprets the bytes in a native-endian fashion.
cloneFromByteArray ::
     Int    -- ^ Offset into byte array, units are elements, not bytes
  -> Nat# n -- ^ Length of the vector, units are elements, not bytes
  -> ByteArray
  -> Vector n Word32#
cloneFromByteArray = Vector.Prim.Word32.unsafeCloneFromByteArray

show :: Nat# n -> Vector n Word32# -> String
show n v = liftShows (\i s -> shows (W32# i) s) n v ""
