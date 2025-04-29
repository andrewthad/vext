{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UnboxedTuples #-}

module Int8
  ( R
  , A#
  , M#
  , empty#
  , index#
  , write#
  , read#
  , unsafeFreeze#
  , initialized#
  , set#
  , unsafeShrinkFreeze#
  , thaw#
  , freeze#
  , copy#
    -- Comparison
  , lt
  , gt
  , eq
  , lt#
  , gt#
  , eq#
  , max
  , min
    -- Metadata
  , size
  ) where

import Prelude hiding (max,min)

import GHC.Exts
import Data.Kind (Type)
import Data.Unlifted (PrimArray#(..),MutablePrimArray#(..))
import EmptyPrimArray (emptyPrimArray#)

import qualified GHC.Exts as Exts

type A# = PrimArray# @'Int8Rep
type M# = MutablePrimArray# @'Int8Rep
type R = 'Int8Rep

unsafeFromI8 :: forall (a :: TYPE 'Int8Rep). Int8# -> a
unsafeFromI8 x = unsafeCoerce# x

unsafeToI8 :: forall (a :: TYPE 'Int8Rep). a -> Int8#
unsafeToI8 x = unsafeCoerce# x

index# :: forall (a :: TYPE R). A# a -> Int# -> a
index# (PrimArray# a) i = unsafeFromI8 (indexInt8Array# a i)

write# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> a -> State# s -> State# s
write# (MutablePrimArray# m) ix a s = writeInt8Array# m ix (unsafeToI8 a) s

read# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> State# s -> (# State# s, a #)
read# (MutablePrimArray# m) ix s = case readInt8Array# m ix s of
  (# s', r #) -> case unsafeFromI8 r of
    r' -> (# s', r' #)

unsafeFreeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> State# s
  -> (# State# s, A# a #)
unsafeFreeze# (MutablePrimArray# m) s0 = case unsafeFreezeByteArray# m s0 of
  (# s1, v #) -> (# s1, PrimArray# v #)

empty# :: forall (a :: TYPE R). (# #) -> A# a
empty# = emptyPrimArray#

initialized# :: forall (s :: Type) (a :: TYPE R).
     Int#
  -> a
  -> State# s
  -> (# State# s, M# s a #)
initialized# n a s0 = case newByteArray# n s0 of
  (# s1, m #) -> case Exts.setByteArray# m 0# n (Exts.int8ToInt# (unsafeToI8 a)) s1 of
    s2 -> (# s2, MutablePrimArray# m #)

set# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> a
  -> State# s
  -> State# s
set# (MutablePrimArray# m) off0 len0 a s0 = Exts.setByteArray# m off0 len0 (Exts.int8ToInt# (unsafeToI8 a)) s0

-- shrink and freeze, all at once
unsafeShrinkFreeze# ::
     M# s a
  -> Int#
  -> State# s
  -> (# State# s, A# a #)
unsafeShrinkFreeze# (MutablePrimArray# m) i s0Alpha = case getSizeofMutableByteArray# m s0Alpha of
  (# s0, sz #) -> case sz ==# i of
    1# -> case Exts.unsafeFreezeByteArray# m s0 of
      (# s1, v #) -> (# s1, PrimArray# v #)
    _ -> case Exts.shrinkMutableByteArray# m i s0 of
      s1 -> case Exts.unsafeFreezeByteArray# m s1 of
        (# s2, v #) -> (# s2, PrimArray# v #)

thaw# :: forall (s :: Type) (a :: TYPE R).
     A# a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, M# s a #)
thaw# (PrimArray# v) off len s0 = case Exts.newByteArray# len s0 of
  (# s1, m #) -> case Exts.copyByteArray# v off m 0# len s1 of
    s2 -> (# s2, MutablePrimArray# m #)

freeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, A# a #)
freeze# (MutablePrimArray# v) off len s0 = case Exts.newByteArray# len s0 of
  (# s1, m #) -> case Exts.copyMutableByteArray# v off m 0# len s1 of
    s2 -> case Exts.unsafeFreezeByteArray# m s2 of
      (# s3, x #) -> (# s3, PrimArray# x #)

copy# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> A# a
  -> Int#
  -> Int#
  -> State# s
  -> State# s
copy# (MutablePrimArray# m) doff (PrimArray# v) soff len s0 =
  Exts.copyByteArray# v soff m doff len s0

max :: forall (a :: TYPE R). a -> a -> a
{-# inline max #-}
max x y = if gt x y then x else y

min :: forall (a :: TYPE R). a -> a -> a
{-# inline min #-}
min x y = if gt x y then y else x

lt :: forall (a :: TYPE R). a -> a -> Bool
{-# inline lt #-}
lt x y = isTrue# (ltInt8# (unsafeToI8 x) (unsafeToI8 y))

gt :: forall (a :: TYPE R). a -> a -> Bool
{-# inline gt #-}
gt x y = isTrue# (gtInt8# (unsafeToI8 x) (unsafeToI8 y))

eq :: forall (a :: TYPE R). a -> a -> Bool
{-# inline eq #-}
eq x y = isTrue# (eqInt8# (unsafeToI8 x) (unsafeToI8 y))

lt# :: forall (a :: TYPE R). a -> a -> Int#
{-# inline lt# #-}
lt# x y = ltInt8# (unsafeToI8 x) (unsafeToI8 y)

gt# :: forall (a :: TYPE R). a -> a -> Int#
{-# inline gt# #-}
gt# x y = gtInt8# (unsafeToI8 x) (unsafeToI8 y)

eq# :: forall (a :: TYPE R). a -> a -> Int#
{-# inline eq# #-}
eq# x y = eqInt8# (unsafeToI8 x) (unsafeToI8 y)

size :: Int
{-# inline size #-}
size = 1

