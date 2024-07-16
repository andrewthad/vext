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

module ZipVector
  ( zip
  , unzip
  ) where

import Prelude hiding (map,zip,unzip)
import Arithmetic.Types (Nat#)
import Control.Monad.ST (runST)
import Data.Either.Void (pattern LeftVoid#,pattern RightVoid#)

import qualified VectorA as A
import qualified VectorB as B
import qualified VectorC as C
import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Nat as Nat

zip :: (a -> b -> c) -> Nat# n -> A.Vector n a -> B.Vector n b -> C.Vector n c
{-# inline zip #-}
zip f n !va !vb = case Nat.testZero# n of
  LeftVoid# zeq -> C.substitute zeq C.empty
  RightVoid# zlt -> runST $ do
    dst <- C.initialized n (f (A.index va (Fin.construct# zlt Nat.N0#)) (B.index vb (Fin.construct# zlt Nat.N0#)))
    Fin.ascendM_# n
      (\fin -> do
        C.write dst fin (f (A.index va fin) (B.index vb fin))
      )
    C.unsafeFreeze dst

unzip :: (a -> (# b, c #)) -> Nat# n -> A.Vector n a -> (# B.Vector n b, C.Vector n c #)
{-# inline unzip #-}
unzip f n !va = case Nat.testZero# n of
  LeftVoid# zeq -> (# B.substitute zeq B.empty, C.substitute zeq C.empty #)
  RightVoid# zlt ->
    let (x,y) = runST $ case f (A.index va (Fin.construct# zlt Nat.N0#)) of
          (# b0, c0 #) -> do
            dstB <- B.initialized n b0
            dstC <- C.initialized n c0
            Fin.ascendM_# n
              (\fin -> case f (A.index va fin) of
                (# b, c #) -> do
                  B.write dstB fin b
                  C.write dstC fin c
              )
            dstB' <- B.unsafeFreeze dstB
            dstC' <- C.unsafeFreeze dstC
            pure (dstB',dstC')
     in (# x, y #)
