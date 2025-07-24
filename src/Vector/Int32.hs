{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language NumericUnderscores #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

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
  , copySlice
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
  , fromFins
  , weakenFins
  , ascendingFins
    -- * Show
  , show
    -- * Interop with primitive
  , cloneFromByteArray
  ) where

import Prelude hiding (replicate,map,maximum,Bounded,all,show)

import Vector.Std.Int32
import Vector.Ord.Int32
import Vector.Eq.Int32

import Control.Monad.ST (ST,runST)
import GHC.Exts (Int32#)
import GHC.Int (Int(I#),Int32(I32#),Int64(I64#))
import GHC.TypeNats (type (+))
import Arithmetic.Types (Nat#,Fin32#,type (<=#),pattern MaybeFin32Just#)
import Arithmetic.Nat ((<?#),(<=?#),pattern N0#)
import Data.Maybe.Void (pattern JustVoid#)
import Data.Either.Void (pattern LeftVoid#, pattern RightVoid#)
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

ascendingFins :: forall n.
     Nat# n
  -> Vector n (Fin32# n)
{-# noinline ascendingFins #-}
ascendingFins n = case n <=?# Nat.constant# @2147483648 (# #) of
  JustVoid# lte -> case Nat.testZero# n of
    LeftVoid# zeqn -> substitute zeqn empty
    RightVoid# zltn -> runST $ do
      let !zeroFin = Fin.construct32# lte zltn N0#
      dst <- initialized n zeroFin
      let go (ix :: Fin32# n) = do
            write dst (Fin.nativeFrom32# ix) ix
            case Fin.succ32# lte n ix of
              MaybeFin32Just# ix' -> go ix'
              _ -> unsafeFreeze dst
      go zeroFin
  _ -> errorWithoutStackTrace "Vector.Int32.ascendingFins: 32-bit finite numbers cannot be >= 2^31"

-- | Convert a vector of Fin32 into a vector of Int32. This is a no-op
-- since it just forgets the information about the upper bound.
fromFins ::
     Vector n (Fin32# m)
  -> Vector n Int32#
{-# inline fromFins #-}
fromFins = unsafeCoerceVector

toFins :: 
     Nat# m -- ^ upper bound
  -> Nat# n -- ^ vector length
  -> Vector n Int32#
  -> Maybe (Vector n (Fin32# m))
toFins m n !v = if all (\v# -> let w = I32# v# in w >= 0 && fromIntegral @Int32 @Int w < I# (Nat.demote# m)) n v
  then Just (unsafeCoerceVector v)
  else Nothing

weakenFins ::
     (a <=# b)
  -> Vector n (Fin32# a)
  -> Vector n (Fin32# b)
{-# inline weakenFins #-}
weakenFins _ (Vector x) = case expose# x of
  PrimArray# z -> Vector (unsafeConstruct# (PrimArray# z))

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
