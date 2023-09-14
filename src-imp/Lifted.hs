{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language UnboxedTuples #-}

module Lifted
  ( R
  , A#
  , ArrayRep
  , M#
  , empty#
  , index#
  , write#
  , read#
  , unsafeFreeze#
  , uninitialized#
  , initialized#
  , set#
  , unsafeShrinkFreeze#
  , thaw#
  , freeze#
  , copy#
  ) where

import GHC.Exts
import Data.Kind (Type)
import Data.Primitive (SmallArray(..),SmallMutableArray(..))

import qualified GHC.Exts as Exts

type ArrayRep = 'BoxedRep 'Unlifted
type R = 'BoxedRep 'Lifted

type A# :: TYPE ('BoxedRep 'Lifted) -> TYPE ('BoxedRep 'Unlifted)
type A# = SmallArray#

type M# :: Type -> TYPE ('BoxedRep 'Lifted) -> TYPE ('BoxedRep 'Unlifted)
type M# = SmallMutableArray#

index# :: forall (a :: TYPE R). A# a -> Int# -> a
index# a i = case indexSmallArray# a i of
  (# r #) -> r

write# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> a -> State# s -> State# s
write# = writeSmallArray#

read# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> State# s -> (# State# s, a #)
read# = readSmallArray#


unsafeFreeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> State# s
  -> (# State# s, A# a #)
unsafeFreeze# = unsafeFreezeSmallArray#

uninitialized# :: forall (s :: Type) (a :: TYPE R).
     Int#
  -> State# s
  -> (# State# s, M# s a #)
uninitialized# i s = newSmallArray# i errorThunk s

initialized# :: forall (s :: Type) (a :: TYPE R).
     Int#
  -> a
  -> State# s
  -> (# State# s, M# s a #)
initialized# i a s = newSmallArray# i a s

empty# :: forall (a :: TYPE R). (# #) -> A# a
empty# _ = 
  let !(# _, z :: SmallArray# a #) = Exts.runRW#
        (\s0 -> case Exts.newSmallArray# 0# (errorThunk :: a) s0 of
          (# s1, x #) -> Exts.unsafeFreezeSmallArray# x s1
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
unsafeShrinkFreeze# m i s0 = case getSizeofSmallMutableArray# m s0 of
  (# s1, n #) -> case n ==# i of
    1# -> Exts.unsafeFreezeSmallArray# m s1
    _ -> Exts.freezeSmallArray# m 0# i s1

-- makes a copy, does not alias the argument
thaw# :: forall (s :: Type) (a :: TYPE R).
     A# a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, M# s a #)
thaw# = Exts.thawSmallArray#

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "SmallArray: uninitialized element"

freeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, A# a #)
freeze# = Exts.freezeSmallArray#

copy# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> A# a
  -> Int#
  -> Int#
  -> State# s
  -> State# s
copy# m doff v soff len s0 =
  Exts.copySmallArray# v soff m doff len s0
