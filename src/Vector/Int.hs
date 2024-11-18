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
  , all
  , any
  , traverse_
  , itraverse_
  , foldlM
  , foldr
  , ifoldl'
  , ifoldlSlice'
  , replicate
  , empty
  , empty_
  , construct1
  , construct2
  , construct3
  , construct4
  , construct5
  , construct6
  , construct7
  , construct1#
  , construct2#
  , construct3#
  , construct4#
  , construct7#
  , construct1_
  , construct2_
  , construct3_
  , construct4_
  , construct7_
  , append
  , clone
  , cloneSlice
    -- * Ordered
  , unique
  , equals
  , elem
  , findIndexEq
  , maximum
  , maximumSlice
  , maximumSliceInitial
  , bubbleSort
  , bubbleSortSlice
  , bubbleSortSliceInPlace
  , mapEq
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
import Vector.Ord.Int
import Vector.Eq.Int
import Data.Primitive (SmallArray(SmallArray))
import Arithmetic.Unsafe (Nat#(Nat#))

import qualified GHC.Exts as Exts
