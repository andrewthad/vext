{-# language BangPatterns #-}
{-# language KindSignatures #-}
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
  , read
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
  , findIndex
  , map
  , traverse_
  , itraverse_
  , foldlM
  , ifoldl'
  , ifoldlSlice'
  , foldrZip
  , replicate
  , empty
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
    -- * Interop with primitive
  , with
  , toSmallArray
    -- * Interop with lists
  , fromList
    -- * Hide Length
  , vector_
  ) where

import Prelude hiding (replicate,map,all,any,read,Bounded)
import Vector.Std.Lifted

import Arithmetic.Types (Fin#)
import Arithmetic.Unsafe (Nat#(Nat#))
import Control.Monad.ST (runST)
import Data.Kind (Type)
import Data.Primitive (SmallArray(SmallArray))
import GHC.Exts (Int(I#))
import GHC.ST (ST(ST))

import qualified GHC.Exts as Exts
import qualified GHC.TypeNats as GHC
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Fin as Fin

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

read :: forall (s :: Type) (n :: GHC.Nat) (a :: Type).
     MutableVector s n a
  -> Fin# n
  -> ST s a
{-# inline read #-}
read (MutableVector x) i =
  ST (\s0 -> read# x i s0)

fromList :: [a] -> Vector_ a
fromList xs0 = case xs0 of
  [] -> empty_
  a0 : _ -> runST $ do
    let !(I# len) = length xs0
    Nat.with# len $ \sz -> do
      dst <- initialized sz a0
      _ <- Fin.ascendM# sz xs0 $ \ix payload -> case payload of
        a : xs -> do
          write dst ix a
          pure xs
        _ -> errorWithoutStackTrace "vext:Vector.Lifted: implementation mistake"
      Vector dst' <- unsafeFreeze dst
      pure (Vector_ sz dst')
        
