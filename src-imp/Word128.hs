{-# language BangPatterns #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UnboxedTuples #-}

module Word128
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
    -- Primitive
  , size
    -- Comparison
  , eq
  , eq#
  ) where

import GHC.Exts
import Data.Kind (Type)
import Data.Unlifted (PrimArray#(..),MutablePrimArray#(..))
import EmptyPrimArray (emptyPrimArray#)

import qualified GHC.Exts as Exts

type A# = PrimArray# @( 'TupleRep '[ 'Word64Rep, 'Word64Rep ] )
type M# = MutablePrimArray# @( 'TupleRep '[ 'Word64Rep, 'Word64Rep ] )
type R = ( 'TupleRep '[ 'Word64Rep, 'Word64Rep ] )

unsafeFromW128 :: forall (a :: TYPE ( 'TupleRep '[ 'Word64Rep, 'Word64Rep ] )). (# Word64#, Word64# #) -> a
{-# inline unsafeFromW128 #-}
unsafeFromW128 (# x, y #) = unsafeCoerce# (# x, y #)

unsafeToW128 :: forall (a :: TYPE ( 'TupleRep '[ 'Word64Rep, 'Word64Rep ] )). a -> (# Word64#, Word64# #)
{-# inline unsafeToW128 #-}
unsafeToW128 x = unsafeCoerce# x

empty# :: forall (a :: TYPE R). (# #) -> A# a
empty# = emptyPrimArray#

eq# :: forall (a :: TYPE R). a -> a -> Int#
{-# inline eq# #-}
eq# x y = case unsafeToW128 x of
  (# x1, x2 #) -> case unsafeToW128 y of
    (# y1, y2 #) -> case eqWord64# x1 y1 of
      1# -> eqWord64# x2 y2
      _ -> 0#

eq :: forall (a :: TYPE R). a -> a -> Bool
{-# inline eq #-}
eq x y = isTrue# (eq# x y)

thaw# :: forall (s :: Type) (a :: TYPE R).
     A# a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, M# s a #)
thaw# (PrimArray# v) off len s0 = case Exts.newByteArray# (len *# 16# ) s0 of
  (# s1, m #) -> case Exts.copyByteArray# v (off *# 16# ) m 0# (len *# 16# ) s1 of
    s2 -> (# s2, MutablePrimArray# m #)

freeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, A# a #)
freeze# (MutablePrimArray# v) off len s0 = case Exts.newByteArray# (len *# 16# ) s0 of
  (# s1, m #) -> case Exts.copyMutableByteArray# v (off *# 16# ) m 0# (len *# 16# ) s1 of
    s2 -> case Exts.unsafeFreezeByteArray# m s2 of
      (# s3, x #) -> (# s3, PrimArray# x #)

-- shrink and freeze, all at once
unsafeShrinkFreeze# ::
     M# s a
  -> Int# -- number of elements to preserve
  -> State# s
  -> (# State# s, A# a #)
unsafeShrinkFreeze# (MutablePrimArray# m) elemCount s0Alpha =
  let !byteCount = elemCount *# 16#
   in case getSizeofMutableByteArray# m s0Alpha of
        (# s0, sz #) -> case sz ==# byteCount of
          1# -> case Exts.unsafeFreezeByteArray# m s0 of
            (# s1, v #) -> (# s1, PrimArray# v #)
          _ -> case Exts.shrinkMutableByteArray# m byteCount s0 of
            s1 -> case Exts.unsafeFreezeByteArray# m s1 of
              (# s2, v #) -> (# s2, PrimArray# v #)

unsafeFreeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> State# s
  -> (# State# s, A# a #)
unsafeFreeze# (MutablePrimArray# m) s0 = case unsafeFreezeByteArray# m s0 of
  (# s1, v #) -> (# s1, PrimArray# v #)

copy# :: M# s a -> Int# -> A# a -> Int# -> Int# -> State# s -> State# s
{-# inline copy# #-}
copy# (MutablePrimArray# dst) doff (PrimArray# src) soff len =
  Exts.copyByteArray# src (soff *# 16#) dst (doff *# 16#) (len *# 16#)

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
set# m@(MutablePrimArray# b) off0 len0 a s0 = case unsafeToW128 a of
  (# x, y #) | 0## <- Exts.word64ToWord# x, 0## <- Exts.word64ToWord# y -> Exts.setByteArray# b (off0 *# 16# ) (len0 *# 16# ) 0# s0
  _ -> setLoop# m off0 len0 a s0


initialized# :: forall (s :: Type) (a :: TYPE R).
     Int#
  -> a
  -> State# s
  -> (# State# s, M# s a #)
initialized# n a s0 = case newByteArray# (n *# 16# ) s0 of
  (# s1, b #) -> case unsafeToW128 a of
    (# x, y #) | 0## <- Exts.word64ToWord# x, 0## <- Exts.word64ToWord# y -> case Exts.setByteArray# b 0# (n *# 16#) 0# s1 of
      s2 -> (# s2, MutablePrimArray# b #)
    _ -> case setLoop# (MutablePrimArray# b) 0# n a s1 of
      s2 -> (# s2, MutablePrimArray# b #)

#if WORDS_BIGENDIAN

-- TODO: Handle big-endian arch if I ever need to.

#else
index# :: forall (a :: TYPE R). A# a -> Int# -> a
{-# inline index# #-}
index# (PrimArray# arr#) i# = unsafeFromW128
  (# Exts.indexWord64Array# arr# ((2# *# i#) +# 1#)
  ,  Exts.indexWord64Array# arr# (2# *# i#) #)

read# :: forall (s :: Type) (a :: TYPE R). M# s a -> Int# -> State# s -> (# State# s, a #)
{-# inline read# #-}
read# (MutablePrimArray# arr#) i# s0 = case Exts.readWord64Array# arr# ((2# *# i#) +# 1#) s0 of
  (# s1, i0 #) -> case Exts.readWord64Array# arr# (2# *# i#) s1 of
    (# s2, i1 #) -> (# s2, unsafeFromW128 (# i0, i1 #) #)

write# :: forall (s :: Type) (a :: TYPE R). M# s a -> Int# -> a -> State# s -> State# s
{-# inline write# #-}
write# (MutablePrimArray# arr#) i# x s0 = case unsafeToW128 x of
  (# a, b #) -> case Exts.writeWord64Array# arr# ((2# *# i#) +# 1#) a s0 of
    s1 -> case Exts.writeWord64Array# arr# (2# *# i#) b s1 of
      s2 -> s2
#endif

size :: Int 
size = 16
