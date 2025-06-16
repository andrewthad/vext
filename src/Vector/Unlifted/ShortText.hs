{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}

module Vector.Unlifted.ShortText
  ( concat
  , toByteArrays
  ) where

import GHC.Exts (ByteArray#)
import Prelude hiding (concat)
import Arithmetic.Types (Nat#)
import Vector.Unlifted (Vector)
import Data.Unlifted (ShortText#(ShortText#))

import qualified Vector.Unlifted as V
import qualified Vector.Unlifted.ByteArray as VUB

concat ::
     Nat# n
  -> Vector n ShortText#
  -> ShortText#
{-# inline concat #-}
concat n !v = ShortText# (VUB.concat n (V.unsafeCoerceVector v))

toByteArrays :: Vector n ShortText# -> Vector n ByteArray#
{-# inline toByteArrays #-}
toByteArrays = V.unsafeCoerceVector
