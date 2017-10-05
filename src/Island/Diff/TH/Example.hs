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
