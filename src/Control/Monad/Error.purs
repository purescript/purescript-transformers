module Control.Monad.Error where

import Control.Alt
import Control.Alternative
import Control.Plus
import Data.Either

class Error a where
  noMsg :: a
  strMsg :: String -> a

instance errorString :: Error String where
  noMsg = ""
  strMsg = id

instance errorEitherAlt :: (Error e) => Alt (Either e) where
  (<|>) (Left _) n = n
  (<|>) m _ = m

instance errorEitherPlus :: (Error e) => Plus (Either e) where
  empty = Left noMsg

instance errorEitherAlternative :: (Error e) => Alternative (Either e)
