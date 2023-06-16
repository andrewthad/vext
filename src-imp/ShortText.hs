{-# language BangPatterns #-}
{-# language MagicHash #-}

module ShortText
  ( E
  , eq#
  , eq
  ) where

import GHC.Exts
import Data.Unlifted (ShortText#(ShortText#))

type E = ShortText#

eq# :: ShortText# -> ShortText# -> Int#
eq# (ShortText# a) (ShortText# b) = case sz ==# sizeofByteArray# b of
  0# -> 0#
  _ -> case compareByteArrays# a 0# b 0# sz of
    0# -> 1#
    _ -> 0#
  where
  !sz = sizeofByteArray# a

eq :: ShortText# -> ShortText# -> Bool
eq a b = isTrue# (eq# a b)
