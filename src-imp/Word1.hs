{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language StandaloneKindSignatures #-}
{-# language UnboxedTuples #-}

-- Note: In this module, we assume that Word is a 64-bit number.
-- It is not hard to correct this assumption with CPP, but this will
-- not be done until someones needs to run this on a 32-bit platform.
--
-- Note: A slightly longer-term goal is to preserve an invariant that
-- any unused trailing bits (there are between 0 and 63 of them in
-- any bit vector) are all zero. Currently, their values are undefined.
-- If they are all zero, then certain read-only operations become
-- faster. For example:
--
-- * Testing that two bit vectors are equal
-- * Testing if any bit in a bit vector is set to true
--
-- Some operations that produce vectors become slower, like @complement@
-- and @initialized@. Others, like @zipAnd@ and @zipOr@, naturally
-- preserve the invariant and do not require special code for the tail. 
--
-- If we start trying to preserve this invariant, we need to first write
-- failing tests that show where the invariant is not currently upheld.
-- At the least, @initialized@ and @unsafeShrinkFreeze@ need changes.
module Word1
  ( R
  , A#
  , M#
  , eq
  , eq#
  , empty#
  , index#
  , write#
  , read#
  , unsafeFreeze#
  , initialized#
  , set#
  , unsafeShrinkFreeze#
  , thaw#
  ) where

import GHC.Exts
import Data.Kind (Type)
import Data.Unlifted (PrimArray#(..),MutablePrimArray#(..))
import EmptyPrimArray (emptyPrimArray#)

import qualified GHC.Exts as Exts

type A# = PrimArray# @'WordRep
type M# = MutablePrimArray# @'WordRep
type R = 'WordRep

eq :: forall (a :: TYPE R). a -> a -> Bool
{-# inline eq #-}
eq x y = isTrue# (eqWord# (unsafeToWord x) (unsafeToWord y))

eq# :: forall (a :: TYPE R). a -> a -> Int#
{-# inline eq# #-}
eq# x y = eqWord# (unsafeToWord x) (unsafeToWord y)

-- Precondition:
-- Argument is not negative
-- Postconditions:
-- First element is >=0, <n/64
-- Second element is >=0, <64
splitIndex_ :: Int# -> (# Int#, Int# #)
{-# inline splitIndex_ #-}
splitIndex_ bitIx = (# wordIx, intraWordIx #)
  where
  wordIx = bitIx `uncheckedIShiftRL#` 6#
  intraWordIx = bitIx `andI#` 0x3F#

-- Precondition: argument must be 0 or 1.
unsafeFromWord :: forall (a :: TYPE 'WordRep). Word# -> a
unsafeFromWord x = unsafeCoerce# x

unsafeToWord :: forall (a :: TYPE 'WordRep). a -> Word#
unsafeToWord x = unsafeCoerce# x

internalIndex# :: ByteArray# -> Int# -> Word#
internalIndex# arr i =
  let !(# wordIx, intraWordIx #) = splitIndex_ i
      !bitBundle = Exts.indexWordArray# arr wordIx
   in unsafeFromWord ((bitBundle `uncheckedShiftRL#` intraWordIx) `and#` 1## )

index# :: forall (a :: TYPE R). A# a -> Int# -> a
index# (PrimArray# arr) i = unsafeFromWord (internalIndex# arr i)

read# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> State# s -> (# State# s, a #)
read# (MutablePrimArray# m) i st =
  let !(# wordIx, intraWordIx #) = splitIndex_ i
      !(# st', bitBundle #) = Exts.readWordArray# m wordIx st
   in (# st', unsafeFromWord ((bitBundle `uncheckedShiftRL#` intraWordIx) `and#` 1## ) #)


write# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> a -> State# s -> State# s
write# (MutablePrimArray# arr) i v st =
  internalWrite# arr i (unsafeToWord v) st

internalWrite# :: forall (s :: Type).
  MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
internalWrite# arr i v st =
  let !(# wordIx, intraWordIx #) = splitIndex_ i
      !(# st', bitBundle #) = Exts.readWordArray# arr wordIx st
      !mask = not# (1## `uncheckedShiftL#` intraWordIx)
      !bitBundle' = (bitBundle `and#` mask) `or#` (v `uncheckedShiftL#` intraWordIx)
   in Exts.writeWordArray# arr wordIx bitBundle' st'

empty# :: forall (a :: TYPE R). (# #) -> A# a
empty# = emptyPrimArray#

unsafeFreeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> State# s
  -> (# State# s, A# a #)
unsafeFreeze# (MutablePrimArray# m) s0 = case unsafeFreezeByteArray# m s0 of
  (# s1, v #) -> (# s1, PrimArray# v #)

-- 
-- 
-- unsafeFreeze# :: forall (s :: Type) (a :: TYPE R).
--      M# s a
--   -> State# s
--   -> (# State# s, A# a #)
-- unsafeFreeze# (MutablePrimArray# m) s0 = case unsafeFreezeByteArray# m s0 of
--   (# s1, v #) -> (# s1, PrimArray# v #)
-- 
-- empty# :: forall (a :: TYPE R). (# #) -> A# a
-- empty# = emptyPrimArray#
-- 
--   (# s1, m #) -> case Exts.setByteArray# m 0# n (Exts.word2Int# (Exts.word8ToWord# (unsafeToW8 a))) s1 of
--     s2 -> (# s2, MutablePrimArray# m #)
-- 
-- set# :: forall (s :: Type) (a :: TYPE R).
--      M# s a
--   -> Int#
--   -> Int#
--   -> a
--   -> State# s
--   -> State# s
-- set# (MutablePrimArray# m) off0 len0 a s0 = Exts.setByteArray# m off0 len0 (Exts.word2Int# (Exts.word8ToWord# (unsafeToW8 a))) s0
-- 

-- shrink and freeze, all at once
unsafeShrinkFreeze# ::
     M# s a
  -> Int#
  -> State# s
  -> (# State# s, A# a #)
unsafeShrinkFreeze# (MutablePrimArray# marr) sz st0 =
  case getSizeofMutableByteArray# marr st0 of
    (# st, oldSzBytes #) ->
      let !(# wordSz, subWordSz #) = splitIndex_ sz
          !paddedSz = wordSz +# if isTrue# (subWordSz ==# 0#) then 0# else 1#
          !szBytes = paddedSz *# 8#
          !st' = case szBytes ==# oldSzBytes of
            1# -> st
            _ -> Exts.shrinkMutableByteArray# marr szBytes st
       in case Exts.unsafeFreezeByteArray# marr st' of
            (# st'', v #) -> (# st'', PrimArray# v #)

-- thaw# :: forall (s :: Type) (a :: TYPE R).
--      A# a
--   -> Int#
--   -> Int#
--   -> State# s
--   -> (# State# s, M# s a #)
-- thaw# (PrimArray# v) off len s0 = case Exts.newByteArray# len s0 of
--   (# s1, m #) -> case Exts.copyByteArray# v off m 0# len s1 of
--     s2 -> (# s2, MutablePrimArray# m #)

set# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> a
  -> State# s
  -> State# s
{-# inline set# #-}
set# parr@(MutablePrimArray# arr) off0 len0 v st0 =
    let subOff = off0 `andI#` 7#
      -- set non-byte-aligned, initial bits
        len = min# len0 (8# -# subOff)
        st' = bitLoop off0 len st0
        -- set full bytes
        off' = off0 +# len
        len' = len0 -# len
        st'' = writeBytes off' len' st'
        -- set trailing bits smaller than a byte
        off'' = off' +# ((len' `uncheckedIShiftRL#` 3#) `uncheckedIShiftL#` 3#)
        len'' = len' `andI#` 7#
     in bitLoop off'' len'' st''
  where
  -- TODO could split bitLoop into writeBitsUnaligned and writeBitsAligned, which would use masking instead of a loop
  bitLoop _ 0# st = st
  bitLoop off len st =
    let st' = write# parr off v st
     in bitLoop (off +# 1#) (len -# 1#) st'
  writeBytes off len st =
    let !offB = off `uncheckedIShiftRL#` 3#
        !lenB = len `uncheckedIShiftRL#` 3#
     in Exts.setByteArray# arr offB lenB vB st
  vB = case unsafeToWord v of
    0## -> 0#
    _ -> 0xFF#

-- TODO: Zero out any trailing bits. 
initialized# :: forall (s :: Type) (a :: TYPE R).
     Int#
  -> a
  -> State# s
  -> (# State# s, M# s a #)
initialized# sz v0 st =
  let !(# wordSz, subWordSz #) = splitIndex_ sz
      !paddedSz = wordSz +# if isTrue# (subWordSz ==# 0#) then 0# else 1#
      !szBytes = paddedSz *# 8#
      !(# st', marr #) = Exts.newByteArray# szBytes st
      !v = case unsafeToWord v0 of
        0## -> 0#
        _ -> 0xFF#
      !st'' = Exts.setByteArray# marr 0# szBytes v st'
   in (# st'', MutablePrimArray# marr #)

min# :: Int# -> Int# -> Int#
{-# inline min# #-}
min# a b = if isTrue# (a <# b) then a else b

copy# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> A# a -> Int# -> Int# -> State# s -> State# s
copy# (MutablePrimArray# dst) doff (PrimArray# src) soff len st =
  internalCopy# dst doff src soff len st

internalCopy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
{-# inline copy# #-}
internalCopy# dst 0# src 0# len st =
-- TODO when soff == doff, we can do like set#
-- first align with naiveCopy, then copy by bytes, then copy the traling bits with naiveCopy
-- in fact, this can work even when soff - doff divisible by 8
  let !lenB = len `uncheckedIShiftRL#` 3#
      !st' = Exts.copyByteArray# src 0# dst 0# lenB st
      !off' = lenB `uncheckedIShiftL#` 3#
      !len' = len `andI#` 7#
   in internalNaiveCopy# dst off' src off' len' st'
internalCopy# dst doff src soff len st = internalNaiveCopy# dst doff src soff len st

internalNaiveCopy# :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> State# s -> State# s
-- TODO if I had an index64 :: ByteArray# -> off:Int#  -> len:Int# -> Int#
-- that reads up to `min len 64` unaligned bits starting at off
-- then I could write whole words at a time after aligning the doff, just as in set#
internalNaiveCopy# _ _ _ _ 0# st = st
internalNaiveCopy# dst doff src soff len st =
  let !v = internalIndex# src soff
      !st' = internalWrite# dst doff v st
   in internalNaiveCopy# dst (doff +# 1#) src (soff +# 1#) (len -# 1#) st'

-- This should be rewritten, but it works for now. At least its
-- correctness is clear.
thaw# :: forall (s :: Type) (a :: TYPE R).
     A# a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, M# s a #)
thaw# v off len s0 = case initialized# len (unsafeFromWord 0## ) s0 of
  (# s1, m #) -> case copy# m 0# v off len s1 of
    s2 -> (# s2, m #)
