{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}
{-# language UnliftedNewtypes #-}

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
  , Vector_(..)
  , FromMutability#
  , vector_
    -- * Primitives
  , C.write#
  , C.write
  , C.read#
  , C.index#
  , C.index
  , unlift
  , C.substitute
  , C.substitute#
  , C.initialized
  , C.initialized#
  , C.empty#
  , C.empty
  , C.unsafeCoerceLength
  , C.unsafeCoerceVector
  , unsafeConstruct#
  , C.expose
  , C.expose#
  , C.freezeSlice
  , C.freezeSlice#
    -- * Ranges
  , set
  , C.setSlice
    -- * Freeze
  , C.unsafeShrinkFreeze
  , C.unsafeFreeze
  , freeze
  , freeze#
    -- * Copy
  , thaw
  , C.thawSlice
    -- * Composite
  , empty_
  , map
  , all
  , any
  , findIndex
  , traverse_
  , traverseZip_
  , traverseST#
  , itraverse_
  , itraverse_#
  , foldlM
  , foldrZip
  , foldr
  , ifoldr
  , ifoldl'
  , ifoldlSlice'
  , liftShows
  , replicate
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
  , C.copySlice
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
  ) where

import Prelude hiding (read,map,Bounded,replicate,all,any,foldr)

import Types (Mutability(Mutable,Immutable))
import Core (Vector(..),Vector#,MutableVector(..),unsafeFreeze,index,write)
import Data.Unlifted (Maybe#(..))
import Rep (R)
import Element (A#,M#)
import GHC.Exts (Int(I#),RuntimeRep(BoxedRep),Levity(Unlifted))
import GHC.ST (ST,runST)
import Data.Kind (Type)
import GHC.Exts (TYPE,State#,Int#,(*#))
import Arithmetic.Unsafe (Fin#(Fin#))
import Arithmetic.Types (type (<),type (<#),Fin(Fin),Nat#)
import Arithmetic.Types (type (<=))
import Arithmetic.Types (type (<=#))
import Arithmetic.Nat ((<?#))
import Arithmetic.Nat (pattern N0#, pattern N1#, pattern N2#, pattern N3#, pattern N4#, pattern N7#)
import GHC.TypeNats (type (+),CmpNat)
import Data.Either.Void (pattern LeftVoid#,pattern RightVoid#)
import Arithmetic.Types (MaybeFin#,pattern MaybeFinNothing#,pattern MaybeFinJust#)
import Data.Maybe.Void (pattern JustVoid#)

import qualified Arithmetic.Equal as Equal
import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Plus as Plus
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

-- | A vector in which the length is hidden.
data Vector_ :: TYPE R -> Type where
  Vector_ :: forall (a :: TYPE R) (n :: GHC.Nat).
       (Nat# n)
    -> (Vector# n a)
    -> Vector_ a

-- | A type family that helps the user define data types that support
-- both mutable and immutable vectors.
type FromMutability# :: Mutability -> (GHC.Nat -> TYPE R -> TYPE ('BoxedRep 'Unlifted))
type family FromMutability# m :: (GHC.Nat -> TYPE R -> TYPE ('BoxedRep 'Unlifted)) where
  FromMutability# 'Immutable = C.Vector#
  FromMutability# ('Mutable s) = C.MutableVector# s

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
    let callback :: forall (j :: GHC.Nat). (j <# i + n) -> Nat# j -> b
        callback lt ix = case C.index v (Fin.construct# (Lt.unlift (Lt.transitiveNonstrictR (Lt.lift lt) p)) ix) of
          a0 -> f b fin a0
     in Fin.with# fin callback

ifoldl' :: forall (n :: GHC.Nat) (a :: TYPE R) (b :: Type).
     (b -> Fin# n -> a -> b)
  -> b
  -> Nat# n
  -> Vector n a
  -> b
{-# inline ifoldl' #-}
ifoldl' f b0 n v = ifoldlSlice' (Lte.reflexive @n) f b0 v (Nat.zero# (# #)) n

foldr :: forall (n :: GHC.Nat) (a :: TYPE R) (b :: Type).
     (a -> b -> b)
  -> b
  -> Nat# n
  -> Vector n a
  -> b
{-# inline foldr #-}
foldr f b0 n v = Fin.descend# n b0 (\fin b -> f (index v fin) b)

liftShows :: forall (n :: GHC.Nat) (a :: TYPE R).
     (a -> String -> String)
  -> Nat# n
  -> Vector n a
  -> String
  -> String
liftShows f n v s0 = case foldr (\e acc -> ',' : f e acc) (']' : s0) n v of
  s@(']' : _) -> '[' : s
  ',' : s -> '[' : s
  _ -> errorWithoutStackTrace "vext.Vector.liftShow: impossible"

ifoldr :: forall (n :: GHC.Nat) (a :: TYPE R) (b :: Type).
     (Fin# n -> a -> b -> b)
  -> b
  -> Nat# n
  -> Vector n a
  -> b
{-# inline ifoldr #-}
ifoldr f b0 n v = Fin.descend# n b0 (\fin b -> f fin (index v fin) b)

traverse_ :: forall (n :: GHC.Nat) (m :: Type -> Type) (a :: TYPE R) (b :: Type).
     Monad m
  => (a -> m b)
  -> Nat# n
  -> Vector n a
  -> m ()
{-# inline traverse_ #-}
traverse_ f n v = Fin.ascendM_# n
  (\fin -> f (index v fin)
  )

traverseZip_ :: forall (n :: GHC.Nat) (m :: Type -> Type) (a :: TYPE R) (b :: TYPE R) (c :: Type).
     Monad m
  => (a -> b -> m c)
  -> Nat# n
  -> Vector n a
  -> Vector n b
  -> m ()
{-# inline traverseZip_ #-}
traverseZip_ f n v w = Fin.ascendM_# n
  (\fin -> f (index v fin) (index w fin)
  )

traverseST# :: forall (n :: GHC.Nat) (s :: Type) (a :: TYPE R) (b :: TYPE R).
     (a -> State# s -> (# State# s, b #))
  -> Nat# n
  -> Vector# n a
  -> State# s
  -> (# State# s, Vector# n b #)
{-# inline traverseST# #-}
traverseST# f n v s0 = case Nat.testZero# n of
  LeftVoid# zeq -> (# s0, C.substitute# zeq (C.empty# (# #)) #)
  RightVoid# zlt ->
    let !(# s1, b0 #) = f (C.index# v (Fin.construct# zlt Nat.N0#)) s0 in
    let !(# s2, dst #) = C.initialized# n b0 s1 in
    let go :: forall p. Nat# p -> State# s -> (# State# s, Vector# n b #)
        go m t0 = case m <?# n of
          JustVoid# lt -> case f (C.index# v (Fin.construct# lt m)) t0 of
            (# t1, e #) -> case C.write# dst (Fin.construct# lt m) e t1 of
              t2 -> go (Nat.succ# m) t2
          _ -> C.unsafeFreeze# dst t0
     in go Nat.N1# s2

itraverse_ :: forall (n :: GHC.Nat) (m :: Type -> Type) (a :: TYPE R) (b :: Type).
     Monad m
  => (Fin# n -> a -> m b)
  -> Nat# n
  -> Vector n a
  -> m ()
{-# inline itraverse_ #-}
itraverse_ f n v = Fin.ascendM_# n
  (\fin -> f fin (index v fin)
  )

itraverse_# :: forall (n :: GHC.Nat) (m :: Type -> Type) (a :: TYPE R) (b :: Type).
     Monad m
  => (Fin# n -> a -> m b)
  -> Nat# n
  -> Vector# n a
  -> m ()
{-# inline itraverse_# #-}
itraverse_# f n v = Fin.ascendM_# n
  (\fin -> f fin (C.index# v fin)
  )

foldrZip :: forall (n :: GHC.Nat) (a :: TYPE R) (b :: TYPE R) (c :: Type).
     (a -> b -> c -> c)
  -> c
  -> Nat# n
  -> Vector n a
  -> Vector n b
  -> c
{-# inline foldrZip #-}
foldrZip f c0 n v1 v2 = Fin.descend# n c0 (\fin c -> f (index v1 fin) (index v2 fin) c)

foldlM :: forall (n :: GHC.Nat) (m :: Type -> Type) (a :: TYPE R) (b :: Type).
     Monad m
  => (b -> a -> m b)
  -> b
  -> Nat# n
  -> Vector n a
  -> m b
{-# inline foldlM #-}
foldlM f b0 n v = Fin.ascendM# n b0
  (\fin acc -> f acc (index v fin)
  )

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
      let callback :: forall (j :: GHC.Nat). (j <# n) -> Nat# j -> ST s ()
          callback lt ix = case C.index v (Fin.construct# (Lt.unlift (Lt.decrementR @n (Lt.substituteL (Equal.symmetric (Plus.associative @j @i @n)) (Lt.substituteR (Plus.commutative @n @m) (Lt.plus (Lt.lift lt) (Lte.lift p)))))) (Nat.plus# ix off0)) of
            a0 -> C.write dst fin (f a0)
      Fin.with# fin callback
    C.unsafeFreeze dst

freeze :: 
     Nat# n -- ^ Mutable vector length
  -> MutableVector s n a -- ^ Mutable vector
  -> ST s (Vector n a)
{-# inline freeze #-}
freeze n mv = C.freezeSlice (Lte.reflexive# (# #)) mv (Nat.zero# (# #)) n

freeze# :: 
     Nat# n -- ^ Mutable vector length
  -> C.MutableVector# s n a -- ^ Mutable vector
  -> State# s
  -> (# State# s, Vector# n a #)
{-# inline freeze# #-}
freeze# n mv = C.freezeSlice# (Lte.reflexive# (# #)) mv (Nat.zero# (# #)) n

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

all :: (a -> Bool) -> Nat# n -> Vector n a -> Bool
{-# inline all #-}
all g n v = Fin.descend# n True (\fin acc -> g (index v fin) && acc)

any :: (a -> Bool) -> Nat# n -> Vector n a -> Bool
{-# inline any #-}
any g n v = Fin.descend# n False (\fin acc -> g (index v fin) || acc)

unlift :: Vector n a -> Vector# n a
unlift (Vector x) = x

construct5 :: a -> a -> a -> a -> a -> Vector 5 a
construct5 x0 x1 x2 x3 x4 = runST $ do
  dst <- C.initialized (Nat.constant# @5 (# #)) x0
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @1 (# #))) x1
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @2 (# #))) x2
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @3 (# #))) x3
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @4 (# #))) x4
  C.unsafeFreeze dst

construct6 :: a -> a -> a -> a -> a -> a -> Vector 6 a
construct6 x0 x1 x2 x3 x4 x5 = runST $ do
  dst <- C.initialized (Nat.constant# @6 (# #)) x0
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @1 (# #))) x1
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @2 (# #))) x2
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @3 (# #))) x3
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @4 (# #))) x4
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @5 (# #))) x5
  C.unsafeFreeze dst

construct7 :: a -> a -> a -> a -> a -> a -> a -> Vector 7 a
construct7 x0 x1 x2 x3 x4 x5 x6 = runST $ do
  dst <- C.initialized (Nat.constant# @7 (# #)) x0
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @1 (# #))) x1
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @2 (# #))) x2
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @3 (# #))) x3
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @4 (# #))) x4
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @5 (# #))) x5
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @6 (# #))) x6
  C.unsafeFreeze dst

construct4 :: a -> a -> a -> a -> Vector 4 a
construct4 x0 x1 x2 x3 = runST $ do
  dst <- C.initialized (Nat.constant# @4 (# #)) x0
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @1 (# #))) x1
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @2 (# #))) x2
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @3 (# #))) x3
  C.unsafeFreeze dst

construct2 :: a -> a -> Vector 2 a
construct2 x0 x1 = runST $ do
  dst <- C.initialized (Nat.constant# @2 (# #)) x0
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @1 (# #))) x1
  C.unsafeFreeze dst

construct3 :: a -> a -> a -> Vector 3 a
construct3 x0 x1 x2 = runST $ do
  dst <- C.initialized (Nat.constant# @3 (# #)) x0
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @1 (# #))) x1
  C.write dst (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @2 (# #))) x2
  C.unsafeFreeze dst

construct1_ :: a -> Vector_ a
construct1_ x0 = Vector_ N1# (construct1# x0)

construct2_ :: a -> a -> Vector_ a
construct2_ x0 x1 = Vector_ N2# (construct2# x0 x1)

construct3_ :: a -> a -> a -> Vector_ a
construct3_ x0 x1 x2 = Vector_ N3# (construct3# x0 x1 x2)

construct4_ :: a -> a -> a -> a -> Vector_ a
construct4_ x0 x1 x2 x3 = Vector_ N4# (construct4# x0 x1 x2 x3)

construct7_ :: a -> a -> a -> a -> a -> a -> a -> Vector_ a
construct7_ x0 x1 x2 x3 x4 x5 x6 = Vector_ N7# (construct7# x0 x1 x2 x3 x4 x5 x6)

construct1# :: a -> Vector# 1 a
construct1# x0 = unlift (construct1 x0)

construct2# :: a -> a -> Vector# 2 a
construct2# x0 x1 = unlift (construct2 x0 x1)

construct3# :: a -> a -> a -> Vector# 3 a
construct3# x0 x1 x2 = unlift (construct3 x0 x1 x2)

construct4# :: a -> a -> a -> a -> Vector# 4 a
construct4# x0 x1 x2 x3 = unlift (construct4 x0 x1 x2 x3)

construct7# :: a -> a -> a -> a -> a -> a -> a -> Vector# 7 a
construct7# x0 x1 x2 x3 x4 x5 x6 = unlift (construct7 x0 x1 x2 x3 x4 x5 x6)

construct1 :: a -> Vector 1 a
construct1 x0 = runST $ do
  dst <- C.initialized (Nat.constant# @1 (# #)) x0
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

index4 :: forall n (a :: TYPE R). (CmpNat 4 n ~ 'LT) => Vector n a -> a
index4 !src = C.index src
  (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @4 (# #)) :: Fin# n)

index5 :: forall n (a :: TYPE R). (CmpNat 5 n ~ 'LT) => Vector n a -> a
index5 !src = C.index src
  (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @5 (# #)) :: Fin# n)

index6 :: forall n (a :: TYPE R). (CmpNat 6 n ~ 'LT) => Vector n a -> a
index6 !src = C.index src
  (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @6 (# #)) :: Fin# n)

index7 :: forall n (a :: TYPE R). (CmpNat 7 n ~ 'LT) => Vector n a -> a
index7 !src = C.index src
  (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @7 (# #)) :: Fin# n)

index8 :: forall n (a :: TYPE R). (CmpNat 8 n ~ 'LT) => Vector n a -> a
index8 !src = C.index src
  (Fin.construct# (Lt.constant# (# #)) (Nat.constant# @8 (# #)) :: Fin# n)

-- TODO: Finish writing this. We need to call copy after initializing.
append :: forall n m (a :: TYPE R).
  Nat# n -> Nat# m -> Vector n a -> Vector m a -> Vector (n + m) a
append n m vn vm = case Nat.testZero# n of
  LeftVoid# zeq -> C.substitute (Equal.plusR# @m zeq) vm
  RightVoid# zlt -> case Nat.testZero# m of
    LeftVoid# zeq -> C.substitute (Equal.plusL# @n zeq) vn
    _ -> runST $ do
      dst <- C.initialized (Nat.plus# n m) (C.index vn (Fin.construct# zlt (Nat.zero# (# #))))
      C.copySlice (Lte.weakenR# @m (Lte.reflexive# @n (# #))) (Lte.reflexive# (# #)) dst Nat.N0# vn Nat.N0# n
      C.copySlice (Lte.reflexive# @(n + m) (# #)) (Lte.reflexive# @m (# #)) dst n vm Nat.N0# m
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

-- | This is extremely unsafe. It allows us to create a vector and
-- invent the length. Users are not supposed to use this. It exists
-- so that we can build @with@ functions for arrays that support
-- recovering the length from an array. (All array types except bit
-- vectors support this.)
unsafeConstruct# :: A# a -> Vector# n a
{-# inline unsafeConstruct# #-}
unsafeConstruct# = C.Vector#

vector_ :: Nat# n -> Vector n a -> Vector_ a
{-# inline vector_ #-}
vector_ n (Vector x) = Vector_ n x

empty_ :: Vector_ a
empty_ = Vector_ Nat.N0# (C.empty# (# #))

findIndex :: forall (n :: GHC.Nat) (a :: TYPE R). (a -> Bool) -> Nat# n -> Vector n a -> MaybeFin# n
findIndex f !n !v = go Nat.N0#
  where
  go :: Nat# k -> MaybeFin# n
  go !ix = case ix <?# n of
    JustVoid# lt ->
      let !fin = Fin.construct# lt ix
       in if f (index v fin)
            then MaybeFinJust# fin
            else go (Nat.succ# ix)
    _ -> MaybeFinNothing#
