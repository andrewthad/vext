{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language StandaloneKindSignatures #-}
{-# language UnboxedTuples #-}

module Word8
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
  ) where

import GHC.Exts
import Data.Kind (Type)
import Data.Unlifted (PrimArray#(..),MutablePrimArray#(..))
import EmptyPrimArray (emptyPrimArray#)

import qualified GHC.Exts as Exts

type A# = PrimArray# @'Word8Rep
type M# = MutablePrimArray# @'Word8Rep
type R = 'Word8Rep

unsafeFromW8 :: forall (a :: TYPE 'Word8Rep). Word8# -> a
unsafeFromW8 x = unsafeCoerce# x

unsafeToW8 :: forall (a :: TYPE 'Word8Rep). a -> Word8#
unsafeToW8 x = unsafeCoerce# x

index# :: forall (a :: TYPE R). A# a -> Int# -> a
index# (PrimArray# a) i = unsafeFromW8 (indexWord8Array# a i)

write# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> a -> State# s -> State# s
write# (MutablePrimArray# m) ix a s = writeWord8Array# m ix (unsafeToW8 a) s

read# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> State# s -> (# State# s, a #)
read# (MutablePrimArray# m) ix s = case readWord8Array# m ix s of
  (# s', r #) -> case unsafeFromW8 r of
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
  (# s1, m #) -> case Exts.setByteArray# m 0# n (Exts.word2Int# (Exts.word8ToWord# (unsafeToW8 a))) s1 of
    s2 -> (# s2, MutablePrimArray# m #)

set# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> a
  -> State# s
  -> State# s
set# (MutablePrimArray# m) off0 len0 a s0 = Exts.setByteArray# m off0 len0 (Exts.word2Int# (Exts.word8ToWord# (unsafeToW8 a))) s0

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
