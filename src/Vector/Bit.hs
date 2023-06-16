{-# language MagicHash #-}

module Vector.Bit
  ( -- Types
    Vector(..)
  , Vector#
  , MutableVector(..)
  , MutableVector#
  , Bounded(..)
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
    -- * Freeze
  , unsafeShrinkFreeze
  , unsafeFreeze
    -- * Copy
  , thaw
    -- * Composite
  , map
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
    -- * Equality
  , equals
  ) where

import Prelude ()

import Vector.Std.Word1
import Vector.Eq.Word1 (equals)
