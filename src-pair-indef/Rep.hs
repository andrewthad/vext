{-# language DataKinds #-}
{-# language BangPatterns #-}
{-# language UnliftedDatatypes #-}
{-# language ExistentialQuantification #-}
{-# language GADTSyntax #-}
{-# language UnliftedNewtypes #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language UnboxedTuples #-}
{-# language TypeFamilies #-}

module Rep
  ( R
  ) where

import GHC.Exts

import qualified RepA as A
import qualified RepB as B

type R = 'TupleRep '[A.R, B.R]
