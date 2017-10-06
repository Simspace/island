{-# LANGUAGE GADTs #-}
module Island.Diff.TH.Example where

import Data.Text
import Island.Diff.TH


data User a b = User
  { _userName   :: Text
  , _userAge    :: Int
  , _userExtra1 :: a
  , _userExtra2 :: b
  }
  deriving (Eq, Show)

makeStructuredPatch ''User
makeStructuredPatch ''Maybe
makeStructuredPatch ''Either
