module Types
  ( Mutability(..)
  ) where

import Data.Kind (Type)

data Mutability = Mutable Type | Immutable
