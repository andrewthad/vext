{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Data.Bytes (Bytes)
import Data.Functor.Classes (liftShowsPrec)
import Data.Maybe (isJust)
import Data.Proxy (Proxy(Proxy))
import Data.Word (Word8,Word64)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.QuickCheck ((===),counterexample)
import GHC.Int (Int32(I32#))
import Arithmetic.Types (Nat#)
import GHC.Exts (Int32#)
import Data.Unlifted (Bool#,pattern True#,pattern False#)

import qualified Data.Bytes as Bytes
import qualified Data.List as List
import qualified GHC.Exts as Exts
import qualified Test.Tasty.QuickCheck as TQC
import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Nat as Nat

import qualified Vector.Int32 as Int32
import qualified Vector.Bit as Bit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testGroup "i32"
    [ TQC.testProperty "maximum" $ \a@(I32# a# ) b@(I32# b# ) c@(I32# c# ) d@(I32# d# ) ->
        let v = Int32.maximum (Nat.constant# @4 (# #)) (Lt.constant# (# #)) (Int32.construct4 a# b# c# d#)
         in I32# v === max (max a b) (max c d)
    , TQC.testProperty "map-eq" $ \a@(I32# a# ) b@(I32# b# ) c@(I32# c# ) d@(I32# d# ) ->
        let v0 = Int32.mapEq (Nat.constant# @4 (# #)) b# (Int32.construct4 a# b# c# d#)
            v1 = Bit.construct4
              (unliftBool $ a == b)
              True#
              (unliftBool $ c == b)
              (unliftBool $ d == b)
         in Bit.equals (Nat.constant# @4 (# #)) v0 v1
    , TQC.testProperty "bubble-sort-min" $ \a@(I32# a# ) b@(I32# b# ) c@(I32# c# ) d@(I32# d# ) ->
        let v = Int32.bubbleSort (Nat.constant# @4 (# #)) (Int32.construct4 a# b# c# d#)
            v0 = Int32.index0 v
            v1 = Int32.index1 v
            v2 = Int32.index2 v
            v3 = Int32.index3 v
         in counterexample
              ( "[" ++ show (I32# v0) ++ "," ++ show (I32# v1) ++ "," ++ show (I32# v2) ++ "," ++
                show (I32# v3) ++ "]"
              )
              (I32# v0 === min (min a b) (min c d))
    , TQC.testProperty "bubble-sort-max" $ \a@(I32# a# ) b@(I32# b# ) c@(I32# c# ) d@(I32# d# ) ->
        let v = Int32.bubbleSort (Nat.constant# @4 (# #)) (Int32.construct4 a# b# c# d#)
            v0 = Int32.index0 v
            v1 = Int32.index1 v
            v2 = Int32.index2 v
            v3 = Int32.index3 v
         in counterexample
              ( "[" ++ show (I32# v0) ++ "," ++ show (I32# v1) ++ "," ++ show (I32# v2) ++ "," ++
                show (I32# v3) ++ "]"
              )
              (I32# v3 === max (max a b) (max c d))
    , TQC.testProperty "replicate" $ \(I32# a# ) ->
        let v0 = Int32.construct4 a# a# a# a#
            v1 = Int32.replicate (Nat.constant# @4 (# #)) a#
         in I32# (Int32.index0 v0) == I32# (Int32.index0 v1)
            &&
            I32# (Int32.index1 v0) == I32# (Int32.index1 v1)
            &&
            I32# (Int32.index2 v0) == I32# (Int32.index2 v1)
            &&
            I32# (Int32.index3 v0) == I32# (Int32.index3 v1)
    , TQC.testProperty "clone" $ \(I32# a# ) (I32# b# ) (I32# c# ) (I32# d# ) ->
        let v0 = Int32.construct4 a# b# c# d#
            v1 = Int32.clone (Nat.constant# @4 (# #)) v0
         in Int32.equals (Nat.constant# @4 (# #)) v0 v1
    , TQC.testProperty "construct4" $ \a@(I32# a# ) b@(I32# b# ) c@(I32# c# ) d@(I32# d# ) ->
        let v = Int32.construct4 a# b# c# d#
         in I32# (Int32.index0 v) == a
            &&
            I32# (Int32.index1 v) == b
            &&
            I32# (Int32.index2 v) == c
            &&
            I32# (Int32.index3 v) == d
    , TQC.testProperty "unique" $ \a@(I32# a# ) b@(I32# b# ) c@(I32# c# ) ->
        let v = Int32.construct3 a# b# c# in
        if | a == b, b == c -> case Int32.unique (Nat.constant# @3 (# #)) v of
               Int32.Bounded m _ r -> counterexample (showI32Vector m (Int32.Vector r)) (Nat.demote (Nat.lift m) === 1)
           | a == b -> case Int32.unique (Nat.constant# @3 (# #)) v of
               Int32.Bounded m _ r -> counterexample (showI32Vector m (Int32.Vector r)) (Nat.demote (Nat.lift m) === 2)
           | b == c -> case Int32.unique (Nat.constant# @3 (# #)) v of
               Int32.Bounded m _ r -> counterexample (showI32Vector m (Int32.Vector r)) (Nat.demote (Nat.lift m) === 2)
           | otherwise -> case Int32.unique (Nat.constant# @3 (# #)) v of
               Int32.Bounded m _ r -> counterexample (showI32Vector m (Int32.Vector r)) (Nat.demote (Nat.lift m) === 3)
    ]
  ]

showI32Vector :: Nat# n -> Int32.Vector n Int32# -> String
showI32Vector n v = Int32.ifoldl' (\acc _ w -> acc ++ ", " ++ show (I32# w))  "" v n

unliftBool :: Bool -> Bool#
unliftBool = \case
  True -> True#
  False -> False#
