{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language PatternSynonyms #-}
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

module MapVector
  ( map
  , map#
  ) where

import Prelude hiding (map)
import Arithmetic.Types (Nat#)
import Control.Monad.ST (runST)
import Data.Either.Void (pattern LeftVoid#,pattern RightVoid#)

import qualified VectorA as A
import qualified VectorB as B
import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Nat as Nat

map :: (a -> b) -> Nat# n -> A.Vector n a -> B.Vector n b
{-# inline map #-}
map f n !v = case Nat.testZero# n of
  LeftVoid# zeq -> B.substitute zeq B.empty
  RightVoid# zlt -> runST $ do
    dst <- B.initialized n (f (A.index v (Fin.construct# zlt Nat.N0#)))
    Fin.ascendM_# n
      (\fin -> do
        B.write dst fin (f (A.index v fin))
      )
    B.unsafeFreeze dst

map# :: (a -> b) -> Nat# n -> A.Vector# n a -> B.Vector# n b
{-# inline map# #-}
map# f n xs = case map f n (A.Vector xs) of
  B.Vector b -> b
