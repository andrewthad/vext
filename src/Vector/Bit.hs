{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language PatternSynonyms #-}
{-# language UnboxedTuples #-}

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
  , read
  , read#
  , index#
  , index
  , unlift
  , substitute
  , initialized
  , unsafeCoerceLength
  , expose
  , expose#
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
  , ifoldl'
  , ifoldlSlice'
  , construct1
  , construct2
  , construct3
  , construct4
  , construct5
  , replicate
  , empty
  , empty_
  , empty#
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
    -- * Custom
  , zipAnd
  , zipOr
  , allEqTrue
  ) where

import Prelude hiding (replicate, map, Bounded, all, foldr, read)
import Data.Unlifted (Bool#, pattern True#, pattern False#)

import GHC.ST (ST(ST))
import Vector.Std.Word1
import Vector.Eq.Word1 (equals)
import Arithmetic.Types (Nat#,Fin#)

import qualified Vector.Zip.Bit.Bit.Bit as Zip

read :: MutableVector s n Bool# -> Fin# n -> ST s Bool
{-# inline read #-}
read (MutableVector v) i = ST
  (\s0 -> case read# v i s0 of
    (# s1, x #) -> case x of
      True# -> (# s1, True #)
      _ -> (# s1, False #)
  )

allEqTrue :: Nat# n -> Vector n Bool# -> Bool
allEqTrue n = foldr (\b acc -> case b of {True# -> acc; _ -> False}) True n

zipOr :: 
     Nat# n
  -> Vector n Bool#
  -> Vector n Bool#
  -> Vector n Bool#
zipOr n xs ys = Zip.zip
  ( \x y ->
      if | False# <- x, False# <- y -> False#
         | otherwise -> True#
  ) n xs ys

zipAnd :: 
     Nat# n
  -> Vector n Bool#
  -> Vector n Bool#
  -> Vector n Bool#
zipAnd n xs ys = Zip.zip
  ( \x y ->
      if | True# <- x, True# <- y -> True#
         | otherwise -> False#
  ) n xs ys
