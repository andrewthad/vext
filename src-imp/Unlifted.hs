{-# language BangPatterns #-}
{-# language GADTSyntax #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language UnboxedTuples #-}
{-# language UnliftedNewtypes #-}

module Unlifted
  ( R
  , A#
  , ArrayRep
  , M#
  , empty#
  , index#
  , write#
  , read#
  , size#
  , unsafeFreeze#
  , set#
  , unsafeShrinkFreeze#
  , thaw#
  , initialized#
  , freeze#
  , copy#
  ) where

import GHC.Exts
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerceUnlifted)

import qualified GHC.Exts as Exts

type ArrayRep = 'BoxedRep 'Unlifted
type R = 'BoxedRep 'Unlifted

type A# :: TYPE ('BoxedRep 'Unlifted) -> TYPE ('BoxedRep 'Unlifted)
type A# = Array#

type M# :: Type -> TYPE ('BoxedRep 'Unlifted) -> TYPE ('BoxedRep 'Unlifted)
type M# = MutableArray#

size# :: forall (a :: TYPE R). A# a -> Int#
size# = sizeofArray#

index# :: forall (a :: TYPE R). A# a -> Int# -> a
index# a i = case indexArray# a i of
  (# r #) -> r

write# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> a -> State# s -> State# s
write# = writeArray#

read# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> State# s -> (# State# s, a #)
read# = readArray#

unsafeFreeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> State# s
  -> (# State# s, A# a #)
unsafeFreeze# = unsafeFreezeArray#

initialized# :: forall (s :: Type) (a :: TYPE R).
     Int#
  -> a
  -> State# s
  -> (# State# s, M# s a #)
initialized# i a s = newArray# i a s

-- This implementation is ridiculous, but GHC does not currently give
-- us a way to allocate an empty array of unlifted elements without
-- supplying an element.
empty# :: forall (a :: TYPE R). (# #) -> A# a
empty# _ = 
  let !(# _, z #) = Exts.runRW#
        (\s0 -> case Exts.newByteArray# 0# s0 of
          (# s1, placeholder #) -> case Exts.newArray# 0# (unsafeCoerceUnlifted @_ @a placeholder) s1 of
            (# s2, x #) -> Exts.unsafeFreezeArray# x s2
        )
   in z

set# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> a
  -> State# s
  -> State# s
set# m off0 len0 a s0 =
  let go off len s = case len of
        0# -> s
        _ -> go (off +# 1#) (len -# 1#) (write# m off a s)
   in go off0 len0 s0

-- shrink and freeze, all at once
unsafeShrinkFreeze# ::
     M# s a
  -> Int#
  -> State# s
  -> (# State# s, A# a #)
unsafeShrinkFreeze# m i s0 = case sizeofMutableArray# m ==# i of
  1# -> Exts.unsafeFreezeArray# m s0
  _ -> Exts.freezeArray# m 0# i s0

thaw# :: forall (s :: Type) (a :: TYPE R).
     A# a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, M# s a #)
thaw# = Exts.thawArray#

freeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, A# a #)
freeze# = Exts.freezeArray#

copy# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> A# a
  -> Int#
  -> Int#
  -> State# s
  -> State# s
copy# m doff v soff len s0 =
  Exts.copyArray# v soff m doff len s0
