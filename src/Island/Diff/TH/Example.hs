{-# LANGUAGE GADTs #-}
module Island.Diff.TH.Example where

import Data.Text
import Island.Diff.TH


data User = User
  { _userName :: Text
  , _userAge  :: Int
  }
  deriving (Eq, Show)

makeStructuredPatch ''User


data MaybeUser
  = NothingUser
  | JustUser User
  deriving (Eq, Show)

makeStructuredPatch ''MaybeUser
makeStructuredPatch ''Maybe


data EitherTextInt
  = LeftText Text
  | RightInt Int
  deriving (Eq, Show)

makeStructuredPatch ''EitherTextInt
makeStructuredPatch ''Either
