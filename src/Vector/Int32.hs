{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language NumericUnderscores #-}
{-# language BangPatterns #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Vector.Int32
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
  , construct1
  , construct2
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
  , findIndexEq
  , maximum
  , maximumSlice
  , maximumSliceInitial
  , bubbleSort
  , bubbleSortSlice
  , bubbleSortSliceInPlace
  , mapEq
    -- * Custom
  , cumulativeSum1
  , toFins
    -- * Show
  , show
    -- * Interop with primitive
  , cloneFromByteArray
  ) where

import Prelude hiding (replicate,map,maximum,Bounded,all,show)

import Vector.Std.Int32
import Vector.Ord.Int32
import Vector.Eq.Int32

import Control.Monad.ST (runST)
import GHC.Exts (Int32#)
import GHC.Int (Int(I#),Int32(I32#),Int64(I64#))
import GHC.TypeNats (type (+))
import Arithmetic.Types (Nat#,Fin32#)
import Data.Primitive (ByteArray(ByteArray))
import Data.Unlifted (PrimArray#(PrimArray#))

import qualified GHC.Exts as Exts
import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Nat as Nat
import qualified Data.Primitive as PM
import qualified Vector.Prim.Int32

-- | Crashes if the sum of all the elements exceeds the maximum
cumulativeSum1 ::
     Nat# n
  -> Vector n Int32#
  -> Vector (n + 1) Int32#
cumulativeSum1 n !v = runST $ do
  dst <- initialized (Nat.succ# n) (Exts.intToInt32# 0#)
  _ <- Fin.ascendM# n (0 :: Int64)
    (\fin acc0 -> do
      let x = index v fin
      let !acc1@(I64# acc1# ) = acc0 + I64# (Exts.intToInt64# (Exts.int32ToInt# x))
      if acc1 > 2_147_483_647
        then errorWithoutStackTrace "Vector.Int32.cumulativeSum1: sum > 2^31-1"
        else if acc1 < (-2_147_483_648) 
          then errorWithoutStackTrace "Vector.Int32.cumulativeSum1: sum < -2^31"
          else do
            write dst (Fin.incrementR# Nat.N1# fin) (Exts.intToInt32# (Exts.int64ToInt# acc1#))
            pure acc1
    )
  unsafeFreeze dst

toFins :: 
     Nat# m -- ^ upper bound
  -> Nat# n -- ^ vector length
  -> Vector n Int32#
  -> Maybe (Vector n (Fin32# m))
toFins m n !v = if all (\v# -> let w = I32# v# in w >= 0 && fromIntegral @Int32 @Int w < I# (Nat.demote# m)) n v
  then Just (unsafeCoerceVector v)
  else Nothing

-- | Crashes the program if the range is out of bounds. That is,
-- behavior is always well defined.
--
-- Interprets the bytes in a native-endian fashion.
cloneFromByteArray ::
     Int    -- ^ Offset into byte array, units are elements, not bytes
  -> Nat# n -- ^ Length of the vector, units are elements, not bytes
  -> ByteArray
  -> Vector n Int32#
cloneFromByteArray = Vector.Prim.Int32.unsafeCloneFromByteArray

show :: Nat# n -> Vector n Int32# -> String
show n v = liftShows (\i s -> shows (I32# i) s) n v ""
