{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language PolyKinds #-}
{-# language MagicHash #-}
{-# language KindSignatures #-}

module Types
  ( Mutability(..)
  , LengthWitness(..)
  ) where

import Data.Kind (Type)
import GHC.Exts (TYPE, RuntimeRep(BoxedRep), Levity(Unlifted), Int#, SmallArray#)
import GHC.Exts (sizeofSmallArray#)

data Mutability = Mutable Type | Immutable

class LengthWitness (f :: TYPE r -> TYPE ('BoxedRep 'Unlifted)) where
  lengthWitness :: forall (a :: TYPE r). f a -> Int#

instance LengthWitness SmallArray# where
  lengthWitness = sizeofSmallArray#
