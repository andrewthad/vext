{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}

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
  , tail
  , cons
  , snoc
  , replaceAt
  , findIndex
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
    -- * Custom
  , ascendingFins
    -- * Unsafe
  , unsafeCoerceVector
    -- * Hide Length
  , vector_
    -- * Recover Length
  , length
  ) where

import Prelude hiding (replicate,map,maximum,Bounded,all,show,elem,length,any,all,tail,foldr)

import Arithmetic.Types (Nat#,Fin#,type (<=#),pattern MaybeFinJust#)
import Arithmetic.Unsafe (Nat#(Nat#))
import Data.Either.Void (pattern LeftVoid#, pattern RightVoid#)
import Data.Unlifted (PrimArray#(PrimArray#))
import Foreign.Storable (sizeOf)
import GHC.Int (Int)
import Vector.Eq.Int
import Vector.Ord.Int
import Vector.Std.Int
import Arithmetic.Nat ((<?#),(<=?#),pattern N0#)
import Control.Monad.ST (ST,runST)

import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Nat as Nat
import qualified GHC.Exts as Exts

length :: Vector n a -> Nat# n
{-# inline length #-}
length !v = case expose v of
  PrimArray# x -> case (sizeOf (undefined :: Int)) of
    Exts.I# i -> Nat# (Exts.quotInt# (Exts.sizeofByteArray# x) i)

ascendingFins :: forall n.
     Nat# n
  -> Vector n (Fin# n)
{-# noinline ascendingFins #-}
ascendingFins n = case Nat.testZero# n of
  LeftVoid# zeqn -> substitute zeqn empty
  RightVoid# zltn -> runST $ do
    let !zeroFin = Fin.construct# zltn N0#
    dst <- initialized n zeroFin
    let go (ix :: Fin# n) = do
          write dst ix ix
          case Fin.succ# n ix of
            MaybeFinJust# ix' -> go ix'
            _ -> unsafeFreeze dst
    go zeroFin
