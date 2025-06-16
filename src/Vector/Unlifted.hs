{-# language MagicHash #-}

module Vector.Unlifted
  ( -- Types
    Vector(..)
  , Vector#
  , Vector_(..)
  , MutableVector(..)
  , MutableVector#
  , Bounded(..)
  , FromMutability#
    -- * Primitives
  , write#
  , write
  , read#
  , index#
  , index
  , unlift
  , substitute
  , substitute#
  , initialized
  , initialized#
  , unsafeCoerceLength
    -- * Ranges
  , set
  , setSlice
    -- * Freeze
  , unsafeShrinkFreeze
  , unsafeFreeze
  , freeze
  , freezeSlice
  , freeze#
  , freezeSlice#
    -- * Copy
  , thaw
    -- * Composite
  , tail
  , cons
  , snoc
  , replaceAt
  , any
  , all
  , findIndex
  , map
  , traverse_
  , itraverse_
  , itraverse_#
  , traverseST#
  , foldlM
  , foldr
  , foldrZip
  , ifoldl'
  , ifoldlSlice'
  , replicate
  , empty
  , empty#
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
  , index6
  , index7
  , index8
    -- * Unsafe
  , unsafeCoerceVector
  ) where

import Prelude ()

import Vector.Std.Unlifted
