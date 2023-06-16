{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language StandaloneKindSignatures #-}
{-# language UnboxedTuples #-}

module EmptyPrimArray
  ( emptyPrimArray#
  ) where

import Data.Unlifted (PrimArray#(..))
import GHC.Exts (RuntimeRep,TYPE)

import qualified GHC.Exts as Exts

emptyPrimArray# :: forall (r :: RuntimeRep) (a :: TYPE r). (# #) -> PrimArray# a
emptyPrimArray# _ =
  let !(# _, z #) = Exts.runRW#
        (\s -> case Exts.newByteArray# 0# s of
          (# s', x #) -> Exts.unsafeFreezeByteArray# x s'
        )
   in PrimArray# z
