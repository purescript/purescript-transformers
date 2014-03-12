module Control.Monad.Error where

import Prelude
import Data.Either

class Error a where
  noMsg :: a
  strMsg :: String -> a
  
instance errorString :: Error String where
  noMsg = ""
  strMsg = id

instance errorEitherAlternative :: (Error e) => Alternative (Either e) where
  empty = Left noMsg
  (<|>) (Left _) n = n
  (<|>) m _ = m
