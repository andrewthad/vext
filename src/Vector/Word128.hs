{-# language MagicHash #-}

module Vector.Word128
  ( module X
  , module Y
  , cloneFromByteArray
  ) where

import Vector.Std.Word128 as X
import Vector.Eq.Word128 as Y

import Arithmetic.Types (Nat#)
import Data.Primitive (ByteArray)

import qualified Vector.Prim.Word128

-- | Crashes the program if the range is out of bounds. That is,
-- behavior is always well defined.
--
-- Interprets the bytes in a native-endian fashion.
cloneFromByteArray ::
     Int    -- ^ Offset into byte array, units are elements, not bytes
  -> Nat# n -- ^ Length of the vector, units are elements, not bytes
  -> ByteArray
  -> Vector n a
cloneFromByteArray = Vector.Prim.Word128.unsafeCloneFromByteArray
