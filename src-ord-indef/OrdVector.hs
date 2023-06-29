{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language UnliftedNewtypes #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

-- Turn this on when debugging performance.
-- OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -ddump-cmm -ddump-asm

module OrdVector
  ( unique
  , equals
  , mapEq
    -- * Maximum
  , maximum
  , maximumSlice
  , maximumSliceInitial
    -- * Bubble Sort
  , bubbleSort
  , bubbleSortSlice
  , bubbleSortSliceInPlace
  ) where

import Prelude hiding (Bounded,max,min,maximum)

import Rep (R,eq,max)
import Vector (MutableVector(MutableVector),MutableVector#,Vector,Bounded(Bounded),index,write,write#,thaw,read#,unsafeShrinkFreeze,unsafeFreeze,thawSlice)
import GHC.ST (ST(ST),runST)
import Arithmetic.Types (type (<),Fin(Fin),Nat#)
import Arithmetic.Types (type (:=:),type (<=))
import Arithmetic.Types (type (<#),type (<=#))
import Arithmetic.Nat ((<?),(<?#))
import GHC.TypeNats (type (+))
import GHC.Exts (TYPE,State#)
import Data.Unlifted (Bool#,pattern True#,pattern False#)

import qualified GHC.TypeNats as GHC
import qualified Element
import qualified Rep
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Fin as Fin
import qualified Vector
import qualified Vector as V
import qualified Vector.Std.Word1 as BV

-- | Test every element in the vector for equality with a scalar value. Returns
-- a vector of booleans.
--
-- Note: The performance of this function could be improved by accumulating
-- 8 or 64 results at a time and writing them out all at once. We need to
-- create a safe interface for doing this though.
mapEq :: forall (n :: GHC.Nat) (a :: TYPE R).
     Nat# n -- ^ Source length
  -> a -- ^ Element to compare against
  -> Vector n a -- ^ Source array
  -> BV.Vector n Bool#
mapEq n e v = runST $ do
  dst <- BV.initialized n False#
  Fin.ascendM_# n $ \fin -> if eq e (V.index v fin)
    then BV.write dst fin True#
    else pure ()
  BV.unsafeFreeze dst

-- | Compare two vectors for equality.
--
-- TODO: reexport this instead
equals :: Nat# n -> Vector n a -> Vector n a -> Bool
equals !n !v0 !v1 = Fin.descend (Nat.lift n) True $ \fin acc ->
  eq (index v0 (Fin.unlift fin)) (index v1 (Fin.unlift fin))
  &&
  acc

maximum :: forall (n :: GHC.Nat) (a :: TYPE R).
     Nat# n
  -> (0 <# n)
  -> Vector n a
  -> a
maximum n lt v = maximumSlice (Lte.reflexive# (# #)) lt v (Nat.zero# (# #)) n

-- | Take the maximum element in a slice. The slice must not be empty.
-- This is enforced by the type system.
maximumSlice :: forall (i :: GHC.Nat) (m :: GHC.Nat) (n :: GHC.Nat) (a :: TYPE R).
     (i + n <=# m)
  -> (0 <# n)
  -> Vector m a
  -> Nat# i
  -> Nat# n
  -> a
maximumSlice lte zlt v off0 n = maximumSliceInitial lte
  (index v (Fin.construct# (Lt.transitiveNonstrictR# zlt (Lte.decrementL# @i (Lte.weakenL# @i lte))) (Nat.zero# (# #))))
  v off0 n

-- | Take the maximum element in a slice. This does not require
-- the slice to be non-null. It takes an additional argument to
-- use as the initial accumulator.
maximumSliceInitial :: forall (i :: GHC.Nat) (m :: GHC.Nat) (n :: GHC.Nat) (a :: TYPE R).
     (i + n <=# m)
  -> a -- initial maximum element
  -> Vector m a
  -> Nat# i
  -> Nat# n
  -> a
{-# noinline maximumSliceInitial #-}
maximumSliceInitial lte b0 !v off0 n = go off0 b0
  where
  end :: Nat# (i + n)
  end = Nat.plus# off0 n
  go :: forall k. Nat# k -> a -> a
  {-# noinline go #-}
  go !m !b = case Nat.lift m <? Nat.lift end of
    Nothing -> b
    Just lt -> go
      (Nat.succ# m)
      (max b (index v (Fin.construct# (Lt.transitiveNonstrictR# (Lt.unlift lt) lte) m)))

bubbleSort ::
     Nat# n
  -> Vector n a
  -> Vector n a
bubbleSort n v = bubbleSortSlice
  (Lte.reflexive# (# #)) v (Nat.zero# (# #)) n

bubbleSortSlice ::
     (i + n <=# m)
  -> Vector m a
  -> Nat# i
  -> Nat# n
  -> Vector n a
bubbleSortSlice p v i0 n = runST $ do
  tgt <- thawSlice p v i0 n
  bubbleSortSliceInPlace (Lte.reflexive# (# #)) tgt (Nat.zero# (# #)) n
  unsafeFreeze tgt

bubbleSortSliceInPlace :: forall i n m a s.
     (i + n <=# m)
  -> MutableVector s m a
  -> Nat# i
  -> Nat# n
  -> ST s ()
{-# noinline bubbleSortSliceInPlace #-}
bubbleSortSliceInPlace lte0 (MutableVector tgt) i0 n =
  ST (\s -> (# outer (Nat.demote (Nat.lift n) - 1) s, () #))
  where
  end :: Nat# (i + n)
  end = Nat.plus# i0 n
  outer :: Int -> State# s -> State# s
  outer !countdown so0 = case countdown of
    0 -> so0
    _ ->
      let inner :: Nat# j -> State# s -> State# s
          inner j si0 = case Nat.succ# j <?# end of
            (# | jsuccltm #) ->
              let k0 = Fin.construct# (Lt.transitiveNonstrictR# (Lt.weakenLhsR# @1 jsuccltm) lte0) j
                  k1 = Fin.construct# (Lt.transitiveNonstrictR# jsuccltm lte0) (Nat.succ# j)
               in case read# tgt k0 si0 of
                    (# si1, e0 #) -> case read# tgt k1 si1 of
                      (# si2, e1 #) -> case Rep.gt# e0 e1 of
                        0# -> inner (Nat.succ# j) si2
                        _ -> case write# tgt k1 e0 si2 of
                          si3 -> case write# tgt k0 e1 si3 of
                            si4 -> inner (Nat.succ# j) si4
            (# _ | #) -> outer (countdown - 1) si0
       in inner i0 so0

-- | Collapse adjacent equal elements into a single element. For example:
--
-- > unique [A,B,A,A,B,B,C,A] ==> [A,B,A,B,C,A]
--
-- Note: This should be rewritten as a @uniqueSlice@ function instead, and
-- then @unique@ could be offers as a convenince where the initial offset
-- is zero and the length of the slice matches the length of the vector.
unique :: forall (n :: GHC.Nat) (a :: TYPE R). Nat# n -> Vector n a -> Bounded n a
{-# noinline unique #-}
unique n !v = case Nat.one <? Nat.lift n of
  -- Empty vector and singleton vector get shared rather than
  -- reallocated.
  Nothing -> Bounded n (Lte.reflexive# (# #)) (Vector.unlift v)
  Just oneLt -> runST $ do
    dst <- thaw n v
    go dst
       (Nat.unlift (Nat.constant @1))
       (Nat.unlift (Nat.constant @1))
       (Lte.fromStrict# (Lt.unlift oneLt))
       (Lte.reflexive# @1 (# #))
       (index v
         (Fin.construct# (Lt.transitive# (Lt.constant# @0 @1 (# #)) (Lt.unlift oneLt)) (Nat.zero# (# #)))
       )
  where
  go :: MutableVector s n a
     -> Nat# ixS -> Nat# ixD -> (ixS <=# n) -> (ixD <=# ixS) -> a
     -> ST s (Bounded n a)
  go !dst ixSrc ixDst slte lte prev = case ixSrc <?# n of
    (# _ | #) -> do
      out <- unsafeShrinkFreeze (Lte.transitive# lte slte) dst ixDst
      pure (Bounded ixDst (Lte.transitive# lte slte) (Vector.unlift out))
    (# | lt #) -> case index v (Fin.construct# lt ixSrc) of
      x -> if eq prev x
        then go
          dst
          (Nat.succ# ixSrc)
          ixDst
          (Lte.fromStrictSucc# lt)
          (Lte.weakenR# @1 lte)
          prev
        else do
          write dst (Fin.construct# (Lt.transitiveNonstrictL# lte lt) ixDst) x
          go dst (Nat.succ# ixSrc) (Nat.succ# ixDst) (Lte.fromStrictSucc# lt) (Lte.incrementR# @1 lte) x
