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
{-# language UnboxedSums #-}

-- Turn this on when debugging performance.
-- OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -ddump-cmm -ddump-asm

module EqVector
  ( equals
  , findIndexEq
  ) where

import Prelude hiding (Bounded,max,min,maximum)

import Rep (R,eq)
import Vector (MutableVector(MutableVector),MutableVector#,Vector,Bounded(Bounded),index,write,write#,thaw,read#,unsafeShrinkFreeze,unsafeFreeze)
import GHC.ST (ST(ST),runST)
import Arithmetic.Types (type (<),Fin(Fin),Nat#)
import Arithmetic.Types (type (:=:),type (<=))
import Arithmetic.Types (type (<#),type (<=#))
import Arithmetic.Nat ((<?),(<?#))
import GHC.TypeNats (type (+))
import GHC.Exts (TYPE,State#)
import Data.Unlifted (Bool#,pattern True#,pattern False#)
import Arithmetic.Types (MaybeFin#,pattern MaybeFinNothing#,pattern MaybeFinJust#)
import Data.Maybe.Void (pattern JustVoid#)

import qualified GHC.TypeNats as GHC
import qualified Element
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Fin as Fin
import qualified Vector
import qualified Vector as V

-- | Compare two vectors for equality.
equals :: Nat# n -> Vector n a -> Vector n a -> Bool
equals !n !v0 !v1 = Fin.descend (Nat.lift n) True $ \fin acc ->
  eq (index v0 (Fin.unlift fin)) (index v1 (Fin.unlift fin))
  &&
  acc

findIndexEq :: forall (n :: GHC.Nat) (a :: TYPE R). Nat# n -> a -> Vector n a -> MaybeFin# n
findIndexEq !n !needle !v = go Nat.N0#
  where
  go :: Nat# k -> MaybeFin# n
  go !ix = case ix <?# n of
    JustVoid# lt ->
      let !fin = Fin.construct# lt ix
       in if eq (V.index v fin) needle
            then MaybeFinJust# fin
            else go (Nat.succ# ix)
    _ -> MaybeFinNothing#
