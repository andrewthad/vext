{-# language BangPatterns #-}
{-# language UnboxedSums #-}
{-# language RankNTypes #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PatternSynonyms #-}

module MaskVector
  ( Vector(..)
  , MutableVector(..)
  , write
  , unsafeFreeze
  , initializeAbsent
    -- * Folds
  , foldMap
  ) where

import Prelude hiding (foldMap)

import Control.Applicative (liftA2)
import Data.Kind (Type)
import Data.Unlifted (Bool#,pattern True#,pattern False#,Maybe#(Maybe#))
import Rep (R)
import GHC.Exts (TYPE)
import Arithmetic.Types (Fin#,Nat#)
import Control.Monad.ST (ST)

import qualified Vector as V
import qualified GHC.TypeNats as GHC
import qualified Vector.Std.Word1 as BV
import qualified Arithmetic.Fin as Fin

-- Effectively, all values are optional. A bit vector of booleans is used
-- to indicate that they are absent.
data Vector :: GHC.Nat -> TYPE R -> Type where
  Vector :: BV.Vector# n Bool# -> V.Vector# n a -> Vector n a

data MutableVector :: Type -> GHC.Nat -> TYPE R -> Type where
  MutableVector :: BV.MutableVector# s n Bool# -> V.MutableVector# s n a -> MutableVector s n a

-- | Initialize a masked vector, and mark everything as absent.
-- This requires providing a default value for each slot even
-- though the interpretation of the data structure is that all
-- elements are absent.
initializeAbsent :: forall (s :: Type) (n :: GHC.Nat) (a :: TYPE R).
     Nat# n
  -> a -- ^ Placeholder value for all slots
  -> ST s (MutableVector s n a)
initializeAbsent n a = do
  BV.MutableVector mask <- BV.initialized n False#
  V.MutableVector vals <- V.initialized n a
  pure (MutableVector mask vals)


-- Write the element at the index. Sets the boolean to true to indicate
-- the the slot is now occupied by a meaningful value.
write :: forall (s :: Type) (n :: GHC.Nat) (a :: TYPE R).
  MutableVector s n a -> Fin# n -> a -> ST s ()
write (MutableVector bools vals) ix a = do
  BV.write (BV.MutableVector bools) ix True#
  V.write (V.MutableVector vals) ix a

unsafeFreeze :: forall (s :: Type) (n :: GHC.Nat) (a :: TYPE R).
  MutableVector s n a -> ST s (Vector n a)
unsafeFreeze (MutableVector bools vals) = do
  BV.Vector bools' <- BV.unsafeFreeze (BV.MutableVector bools)
  V.Vector vals' <- V.unsafeFreeze (V.MutableVector vals)
  pure (Vector bools' vals')

foldMap :: forall (n :: GHC.Nat) (a :: TYPE R) (m :: Type).
     Monoid m
  => (Maybe# a -> m)
  -> Nat# n
  -> Vector n a
  -> m
foldMap f n (Vector mask vals) = Fin.descend# n mempty
  (\fin acc -> case BV.index# mask fin of
    True# -> f (Maybe# (# | (V.index# vals fin) #)) <> acc
    _ -> f (Maybe# (# (# #) | #)) <> acc
  )
