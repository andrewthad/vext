{-# language MagicHash #-}

module Vector.Unlifted
  ( -- Types
    Vector(..)
  , Vector#
  , Vector_(..)
  , MutableVector(..)
  , MutableVector#
  , Bounded(..)
  , Vector_(..)
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
  , empty#
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
  , itraverse_
  , itraverse_#
  , traverseST#
  , foldlM
  , foldr
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
