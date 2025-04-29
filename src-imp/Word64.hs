{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UnboxedTuples #-}

module Word64
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

type A# = PrimArray# @'Word64Rep
type M# = MutablePrimArray# @'Word64Rep
type R = 'Word64Rep

unsafeFromW64 :: forall (a :: TYPE 'Word64Rep). Word64# -> a
unsafeFromW64 x = unsafeCoerce# x

unsafeToW64 :: forall (a :: TYPE 'Word64Rep). a -> Word64#
unsafeToW64 x = unsafeCoerce# x

index# :: forall (a :: TYPE R). A# a -> Int# -> a
index# (PrimArray# a) i = unsafeFromW64 (indexWord64Array# a i)

write# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> a -> State# s -> State# s
write# (MutablePrimArray# m) ix a s = writeWord64Array# m ix (unsafeToW64 a) s

read# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> State# s -> (# State# s, a #)
read# (MutablePrimArray# m) ix s = case readWord64Array# m ix s of
  (# s', r #) -> case unsafeFromW64 r of
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
initialized# n a s0 = case newByteArray# (n *# 8# ) s0 of
  (# s1, b #) -> case Exts.word64ToWord# (unsafeToW64 a) of
    0## -> case Exts.setByteArray# b 0# (n *# 8#) 0# s1 of
      s2 -> (# s2, MutablePrimArray# b #)
    _ -> case setLoop# (MutablePrimArray# b) 0# n a s1 of
      s2 -> (# s2, MutablePrimArray# b #)

-- Not exported. Offset and length are counts of elements, not bytes
setLoop# :: forall (s :: Type) (a :: TYPE R). M# s a -> Int# -> Int# -> a -> State# s -> State# s
setLoop# marr off len x s = case len of                                    
  0# -> s
  _ -> setLoop# marr (off +# 1# ) (len -# 1# ) x (write# marr off x s)                         

set# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> a
  -> State# s
  -> State# s
set# m@(MutablePrimArray# b) off0 len0 a s0 = case Exts.word64ToWord# (unsafeToW64 a) of
  0## -> Exts.setByteArray# b (off0 *# 8# ) (len0 *# 8# ) 0# s0
  _ -> setLoop# m off0 len0 a s0

-- shrink and freeze, all at once
unsafeShrinkFreeze# ::
     M# s a
  -> Int# -- number of elements to preserve
  -> State# s
  -> (# State# s, A# a #)
unsafeShrinkFreeze# (MutablePrimArray# m) elemCount s0Alpha =
  let !byteCount = elemCount *# 8#
   in case getSizeofMutableByteArray# m s0Alpha of
        (# s0, sz #) -> case sz ==# byteCount of
          1# -> case Exts.unsafeFreezeByteArray# m s0 of
            (# s1, v #) -> (# s1, PrimArray# v #)
          _ -> case Exts.shrinkMutableByteArray# m byteCount s0 of
            s1 -> case Exts.unsafeFreezeByteArray# m s1 of
              (# s2, v #) -> (# s2, PrimArray# v #)

thaw# :: forall (s :: Type) (a :: TYPE R).
     A# a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, M# s a #)
thaw# (PrimArray# v) off len s0 = case Exts.newByteArray# (len *# 8# ) s0 of
  (# s1, m #) -> case Exts.copyByteArray# v (off *# 8# ) m 0# (len *# 8# ) s1 of
    s2 -> (# s2, MutablePrimArray# m #)

freeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, A# a #)
freeze# (MutablePrimArray# v) off len s0 = case Exts.newByteArray# (len *# 8# ) s0 of
  (# s1, m #) -> case Exts.copyMutableByteArray# v (off *# 8# ) m 0# (len *# 8# ) s1 of
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
  Exts.copyByteArray# v (8# *# soff) m (8# *# doff) (8# *# len) s0

size :: Int
{-# inline size #-}
size = 8

max :: forall (a :: TYPE R). a -> a -> a
{-# inline max #-}
max x y = if gt x y then x else y

min :: forall (a :: TYPE R). a -> a -> a
{-# inline min #-}
min x y = if gt x y then y else x

lt :: forall (a :: TYPE R). a -> a -> Bool
{-# inline lt #-}
lt x y = isTrue# (ltWord64# (unsafeToW64 x) (unsafeToW64 y))

gt :: forall (a :: TYPE R). a -> a -> Bool
{-# inline gt #-}
gt x y = isTrue# (gtWord64# (unsafeToW64 x) (unsafeToW64 y))

eq :: forall (a :: TYPE R). a -> a -> Bool
{-# inline eq #-}
eq x y = isTrue# (eqWord64# (unsafeToW64 x) (unsafeToW64 y))

lt# :: forall (a :: TYPE R). a -> a -> Int#
{-# inline lt# #-}
lt# x y = ltWord64# (unsafeToW64 x) (unsafeToW64 y)

gt# :: forall (a :: TYPE R). a -> a -> Int#
{-# inline gt# #-}
gt# x y = gtWord64# (unsafeToW64 x) (unsafeToW64 y)

eq# :: forall (a :: TYPE R). a -> a -> Int#
{-# inline eq# #-}
eq# x y = eqWord64# (unsafeToW64 x) (unsafeToW64 y)
