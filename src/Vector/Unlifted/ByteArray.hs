{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}
{-# language NumericUnderscores #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}

module Vector.Unlifted.ByteArray
  ( concat
  , lengths32
  ) where

import GHC.Exts (ByteArray#,Int32#,(>#))
import Prelude hiding (concat)
import Arithmetic.Types (Nat#)
import Vector.Unlifted (Vector)
import Data.Primitive (ByteArray(ByteArray))
import Control.Monad.ST.Run (runByteArrayST)

import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Vector.Unlifted as V
import qualified Vector.Int32
import qualified Vector.Map.Unlifted.Int32

concat ::
     Nat# n
  -> Vector n ByteArray#
  -> ByteArray#
{-# noinline concat #-}
concat n !v =
  let !(ByteArray u) = runByteArrayST $ do
        let totalLen = totalLength n v
        dst <- PM.newByteArray totalLen
        !_ <- V.foldlM
          (\dstIx b# -> do
            let b = ByteArray b#
            let len = PM.sizeofByteArray b
            PM.copyByteArray dst dstIx b 0 len
            pure (dstIx + len)
          ) 0 n v
        PM.unsafeFreezeByteArray dst
   in u

-- | Crash the program if any length is greater than what a 32-bit signed
-- integer can represent. 
lengths32 ::
     Nat# n
  -> Vector n ByteArray#
  -> Vector.Int32.Vector n Int32#
{-# noinline lengths32 #-}
lengths32 n !v = Vector.Map.Unlifted.Int32.map
  (\a ->
    let sz = Exts.sizeofByteArray# a
     in case sz ># 2_147_483_647# of
          1# -> errorWithoutStackTrace "Vector.Unlifted.ByteArray.length32: length > 2^31-1"
          _ -> Exts.intToInt32# sz
  ) n v

totalLength :: Nat# n -> Vector n ByteArray# -> Int
totalLength n v = V.ifoldl'
  (\acc _ a -> acc + PM.sizeofByteArray (ByteArray a)
  ) 0 n v
