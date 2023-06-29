{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}

-- The only operatations defined in this module are those
-- that are considered primitive. That is, they cannot be
-- defined in terms of other operations on length-indexed
-- vectors.
module Vector
  ( -- Types
    C.Vector(..)
  , C.Vector#
  , C.MutableVector(..)
  , C.MutableVector#
  , Bounded(..)
    -- * Primitives
  , C.write#
  , C.write
  , C.read#
  , C.index#
  , C.index
  , unlift
  , C.substitute
  , C.initialized
  , C.empty#
  , C.empty
  , C.unsafeCoerceLength
  , C.unsafeCoerceVector
  , C.expose
  , C.expose#
  , C.freezeSlice
    -- * Ranges
  , set
  , C.setSlice
    -- * Freeze
  , C.unsafeShrinkFreeze
  , C.unsafeFreeze
  , freeze
    -- * Copy
  , thaw
  , C.thawSlice
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
  ) where

import Prelude hiding (read,map,Bounded,replicate)

import Core (Vector(..),Vector#,MutableVector(..),unsafeFreeze,index,write)
import Data.Unlifted (Maybe#(..))
import Rep (R)
import Element (A#,M#)
import GHC.Exts (Int(I#),RuntimeRep)
import GHC.ST (ST,runST)
import Data.Kind (Type)
import GHC.Exts (TYPE,State#,Int#,(*#))
import Arithmetic.Unsafe (Fin#(Fin#))
import Arithmetic.Types (type (<),Fin(Fin),Nat#)
import Arithmetic.Types (type (:=:),type (<=))
import Arithmetic.Types (type (<=#))
import GHC.TypeNats (type (+),CmpNat)

import qualified Element as A
import qualified Arithmetic.Equal as Equal
import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Plus as Plus
import qualified Arithmetic.Types as Arithmetic
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Core as C
import qualified GHC.TypeNats as GHC

-- | A vector with a known upper bound on its length but whose exact
-- length is not known.
data Bounded :: GHC.Nat -> TYPE R -> Type where
  Bounded :: forall (a :: TYPE R) (n :: GHC.Nat) (m :: GHC.Nat).
       (Nat# m)
    -> (m <=# n)
    -> (Vector# m a)
    -> Bounded n a

ifoldlSlice' :: forall (i :: GHC.Nat) (m :: GHC.Nat) (n :: GHC.Nat) (a :: TYPE R) (b :: Type).
     (i + n <= m)
  -> (b -> Fin# (i + n) -> a -> b)
  -> b
  -> Vector m a
  -> Nat# i
  -> Nat# n
  -> b
{-# inline ifoldlSlice' #-}
ifoldlSlice' p f b0 v off0 n =
  Fin.ascendFrom'# off0 n b0 $ \fin b ->
    let callback :: forall (j :: GHC.Nat). (j < i + n) -> Nat# j -> b
        callback lt ix = case C.index v (Fin.construct# (Lt.unlift (Lt.transitiveNonstrictR lt p)) ix) of
          a0 -> f b fin a0
     in Fin.with# fin callback

ifoldl' :: forall (n :: GHC.Nat) (a :: TYPE R) (b :: Type).
     (b -> Fin# n -> a -> b)
  -> b
  -> Vector n a
  -> Nat# n
  -> b
{-# inline ifoldl' #-}
ifoldl' f b0 v n = ifoldlSlice' (Lte.reflexive @n) f b0 v (Nat.zero# (# #)) n

-- | Map over a slice of a vector.
mapSlice :: forall (i :: GHC.Nat) (m :: GHC.Nat) (n :: GHC.Nat) (a :: TYPE R).
     (i + n <=# m)
  -> (a -> a)
  -> Vector m a
  -> Nat# i -- start index
  -> Nat# n -- length
  -> Vector n a
{-# inline mapSlice #-}
mapSlice p f v off0 n = runST action where
  -- TODO: We should use Fin.ascendFromM_# to avoid unneeded additions.
  action :: forall s. ST s (Vector n a)
  action = do
    dst <- C.thawSlice p v off0 n
    Fin.ascendM_# n $ \fin -> do
      let callback :: forall (j :: GHC.Nat). (j < n) -> Nat# j -> ST s ()
          callback lt ix = case C.index v (Fin.construct# (Lt.unlift (Lt.decrementR @n (Lt.substituteL (Equal.symmetric (Plus.associative @j @i @n)) (Lt.substituteR (Plus.commutative @n @m) (Lt.plus lt (Lte.lift p)))))) (Nat.plus# ix off0)) of
            a0 -> C.write dst fin (f a0)
      Fin.with# fin callback
    C.unsafeFreeze dst

freeze :: 
     Nat# n -- ^ Mutable vector length
  -> MutableVector s n a -- ^ Mutable vector
  -> ST s (Vector n a)
{-# inline freeze #-}
freeze n mv = C.freezeSlice (Lte.reflexive# (# #)) mv (Nat.zero# (# #)) n

thaw :: 
     Nat# n -- ^ Vector length
  -> Vector n a -- ^ Mutable vector
  -> ST s (MutableVector s n a)
{-# inline thaw #-}
thaw n mv = C.thawSlice (Lte.reflexive# (# #)) mv (Nat.zero# (# #)) n

-- | Set all elements in the mutable vector to the same value.
set :: 
     MutableVector s n a -- ^ Mutable vector
  -> Nat# n -- ^ Vector length
  -> a -- ^ Value
  -> ST s ()
{-# inline set #-}
set mv n a = C.setSlice (Lte.reflexive# (# #)) mv (Nat.zero# (# #)) n a

-- | Map over a vector starting at offset 0.
map :: 
     (a -> a)
  -> Vector n a
  -> Nat# n -- length
  -> Vector n a
{-# inline map #-}
map f v n = mapSlice (Lte.reflexive# (# #)) f v (Nat.zero# (# #)) n

unlift :: Vector n a -> Vector# n a
unlift (Vector x) = x

construct4 :: a -> a -> a -> a -> Vector 4 a
construct4 x0 x1 x2 x3 = runST $ do
  dst <- C.initialized (Nat.constant# @4 (# #)) x0
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @1 (# #))) x1
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @2 (# #))) x2
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @3 (# #))) x3
  C.unsafeFreeze dst

construct3 :: a -> a -> a -> Vector 3 a
construct3 x0 x1 x2 = runST $ do
  dst <- C.initialized (Nat.constant# @3 (# #)) x0
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @1 (# #))) x1
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @2 (# #))) x2
  C.unsafeFreeze dst

replicate :: Nat# n -> a -> Vector n a
replicate n a = runST (C.unsafeFreeze =<< C.initialized n a)

index0 :: forall n (a :: TYPE R). (CmpNat 0 n ~ 'LT) => Vector n a -> a
index0 !src = C.index src
  (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @0 (# #)) :: Fin# n)

index1 :: forall n (a :: TYPE R). (CmpNat 1 n ~ 'LT) => Vector n a -> a
index1 !src = C.index src
  (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @1 (# #)) :: Fin# n)

index2 :: forall n (a :: TYPE R). (CmpNat 2 n ~ 'LT) => Vector n a -> a
index2 !src = C.index src
  (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @2 (# #)) :: Fin# n)

index3 :: forall n (a :: TYPE R). (CmpNat 3 n ~ 'LT) => Vector n a -> a
index3 !src = C.index src
  (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @3 (# #)) :: Fin# n)

-- TODO: Finish writing this. We need to call copy after initializing.
append :: forall n m (a :: TYPE R).
  Nat# n -> Nat# m -> Vector n a -> Vector m a -> Vector (n + m) a
append n m vn vm = case Nat.testZero# n of
  (# zeq | #) -> C.substitute (Equal.plusR# @m zeq) vm
  (# | zlt #) -> case Nat.testZero# m of
    (# zeq | #) -> C.substitute (Equal.plusL# @n zeq) vn
    (# | _ #) -> runST $ do
      dst <- C.initialized (Nat.plus# n m) (C.index vn (Fin.construct# zlt (Nat.zero# (# #))))
      C.unsafeFreeze dst

-- TODO: Add a new primitive to Element for this instead.
cloneSlice :: 
     (i + n <=# m)
  -> Vector m a
  -> Nat# i
  -> Nat# n
  -> Vector n a
cloneSlice lte v off len = runST (C.thawSlice lte v off len >>= C.unsafeFreeze)

clone :: 
     Nat# n
  -> Vector n a
  -> Vector n a
clone len v = runST (C.thawSlice (Lte.reflexive# (# #)) v (Nat.zero# (# #)) len >>= C.unsafeFreeze)
