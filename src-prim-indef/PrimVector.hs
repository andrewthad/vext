{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language KindSignatures #-}

module PrimVector
  ( unsafeCloneFromByteArray
  ) where

import Arithmetic.Types (Nat#)
import Control.Monad.ST (runST)
import Data.Primitive (ByteArray(ByteArray))
import Data.Unlifted (PrimArray#(PrimArray#))
import GHC.Exts (TYPE)
import GHC.Int (Int(I#))
import GHC.TypeNats (Nat)
import Rep (R)
import Vector (Vector)

import qualified Arithmetic.Nat as Nat
import qualified Data.Primitive as PM
import qualified Vector as V
import qualified Element as E

-- | Crashes the program if the range is out of bounds. That is,
-- behavior is always well defined.
--
-- Interprets the bytes in a native-endian fashion.
--
-- This is unsafe because it interprets bytes as an arbitrary
-- type.
unsafeCloneFromByteArray :: forall (n :: Nat) (a :: TYPE R).
     Int    -- ^ Offset into byte array, units are elements, not bytes
  -> Nat# n -- ^ Length of the vector, units are elements, not bytes
  -> ByteArray
  -> Vector n a
unsafeCloneFromByteArray !ix !n !b
  | ix < 0 = errorWithoutStackTrace "PrimVector.cloneFromByteArray: negative offset"
  | ixScaled + nScaled > sz = errorWithoutStackTrace "PrimVector.cloneFromByteArray: slice goes past the end"
  | otherwise =
      let !(ByteArray result) = runST $ do
            dst <- PM.newByteArray nScaled
            PM.copyByteArray dst 0 b ixScaled nScaled
            PM.unsafeFreezeByteArray dst
       in V.Vector (V.unsafeConstruct# (PrimArray# result))
  where
  sz = PM.sizeofByteArray b
  ixScaled = ix * E.size
  nScaled = I# (Nat.demote# n) * E.size
