{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}

module Vector.Lifted
  ( -- Types
    Vector(..)
  , Vector#
  , MutableVector(..)
  , MutableVector#
  , Bounded(..)
  , Vector_(..)
    -- * Primitives
  , write#
  , write
  , read#
  , index#
  , index
  , unlift
  , substitute
  , initialized
  , unsafeCoerceLength
    -- * Ranges
  , set
  , setSlice
    -- * Freeze
  , unsafeShrinkFreeze
  , unsafeFreeze
  , freeze
  , freezeSlice
    -- * Copy
  , thaw
    -- * Composite
  , any
  , all
  , map
  , traverse_
  , foldlM
  , ifoldl'
  , ifoldlSlice'
  , replicate
  , construct1
  , construct3
  , construct4
  , construct5
  , append
  , clone
  , cloneSlice
    -- * Index
  , index0
  , index1
  , index2
  , index3
  , index4
  , index5
    -- * Unsafe
  , unsafeCoerceVector
    -- * Interop with primitive
  , with
  , toSmallArray
    -- * Hide Length
  , vector_
  ) where

import Prelude ()

import Vector.Std.Lifted
import Data.Primitive (SmallArray(SmallArray))
import Arithmetic.Unsafe (Nat#(Nat#))

import qualified GHC.Exts as Exts

with ::
     SmallArray a
  -> (forall n. Nat# n -> Vector n a -> b)
  -> b
{-# inline with #-}
with (SmallArray xs) f =
  f (Nat# (Exts.sizeofSmallArray# xs)) (Vector (unsafeConstruct# xs))

toSmallArray :: Vector n a -> SmallArray a
{-# inline toSmallArray #-}
toSmallArray !v = SmallArray (expose v)
