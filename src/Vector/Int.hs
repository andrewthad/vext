{-# language MagicHash #-}
{-# language RankNTypes #-}

module Vector.Int
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
  , map
  , traverse_
  , foldlM
  , ifoldl'
  , ifoldlSlice'
  , replicate
  , construct3
  , construct4
  , append
  , clone
  , cloneSlice
    -- * Index
  , index0
  , index1
  , index2
  , index3
    -- * Unsafe
  , unsafeCoerceVector
    -- * Hide Length
  , vector_
  ) where

import Prelude ()

import Vector.Std.Int
import Data.Primitive (SmallArray(SmallArray))
import Arithmetic.Unsafe (Nat#(Nat#))

import qualified GHC.Exts as Exts
