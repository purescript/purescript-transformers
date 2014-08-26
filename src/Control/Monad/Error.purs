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

