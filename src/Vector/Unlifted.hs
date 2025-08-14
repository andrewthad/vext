{-# language MagicHash #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language UnboxedTuples #-}

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
  , read
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
  , copySlice
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
  , generateST#
  , foldlM
  , foldr
  , foldrZip
  , ifoldl'
  , ifoldlSlice'
  , replicate
  , empty
  , empty#
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
import Data.Unlifted (Lifted(Lifted))
import Arithmetic.Types (Fin#)
import GHC.Exts (TYPE,RuntimeRep(BoxedRep),Levity(Unlifted))
import Data.Kind (Type)

import Vector.Std.Unlifted
import GHC.ST (ST(ST))

import qualified GHC.TypeNats as GHC

-- | This is the way this function should be used:
--
-- > do { ... ; Lifted val <- read vector ix; ... val }
read :: forall (s :: Type) (n :: GHC.Nat) (a :: TYPE ('BoxedRep 'Unlifted)).
     MutableVector s n a
  -> Fin# n
  -> ST s (Lifted a)
{-# inline read #-}
read (MutableVector x) i =
  ST (\s0 -> case read# x i s0 of { (# s1, v #) -> (# s1, Lifted v #) } )
