module Control.Monad.Error where

class Error a where
  noMsg :: a
  strMsg :: String -> a

instance errorString :: Error String where
  noMsg = ""
  strMsg = id

