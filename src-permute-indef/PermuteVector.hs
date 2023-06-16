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

module PermuteVector
  ( permute
  ) where

import Prelude hiding (Bounded,max,min,maximum)

import Rep (R)
import FinType (Finite#,weaken)
import GHC.ST (ST(ST),runST)
import Arithmetic.Types (type (<),Fin(Fin),Nat#)
import Arithmetic.Types (type (:=:),type (<=))
import Arithmetic.Types (type (<#),type (<=#))
import Arithmetic.Nat ((<?),(<?#))
import GHC.TypeNats (type (+))
import GHC.Exts (TYPE,State#)

import qualified GHC.TypeNats as GHC
import qualified Element
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Fin as Fin
import qualified Vector as V
import qualified FinVector as FV

-- | Permute the source array according to the indices:
--
-- forall ix. output[ix] = source[indices[ix]]
permute :: forall (m :: GHC.Nat) (n :: GHC.Nat) (a :: TYPE R).
     Nat# m -- ^ indices length
  -> FV.Vector m (Finite# n) -- ^ indices
  -> V.Vector n a -- ^ source
  -> V.Vector m a -- ^ output
{-# noinline permute #-}
permute m !ixs !v = case Nat.testZero# m of
  (# zeq | #) -> V.substitute zeq (V.empty (# #))
  (# | zlt #) -> runST $ do
    -- More clean presentation of initialization:  
    -- dst := initialize(v[ixs[0]]])
    dst <- V.initialized m (V.index v (weaken (FV.index ixs (Fin.construct# zlt (Nat.zero# (# #))))))
    Fin.ascendM_# m $ \fin -> do
      V.write dst fin (V.index v (weaken (FV.index ixs fin)))
    V.unsafeFreeze dst
