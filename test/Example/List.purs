module Example.List where

import Prelude

import Control.Monad.List.Trans (ListT, runListTRec, iterate, takeWhile)
import Control.MonadZero (guard)
import Data.Array as A
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

-- based on http://hackage.haskell.org/package/list-transformer
logList :: ListT Effect String -> Effect Unit
logList l = runListTRec do
  liftEffect $ log "logging listT"
  str <- l
  liftEffect $ log str

-- based on https://wiki.haskell.org/ListT_done_right#Sum_of_squares
sumSqrs :: Int -> ListT Effect Unit
sumSqrs n = do
  let
    nats = iterate (add one) zero
    squares = takeWhile (_ <= n) $ map (\x -> x * x) nats
  x <- squares
  y <- squares
  liftEffect $ log ("<" <> show x <> "," <> show y <> ">")
  guard $ x + y == n
  liftEffect $ log "Sum of squares."

main :: Effect Unit
main = do
  logList $ A.toUnfoldable ["one", "two", "three"]
  runListTRec $ sumSqrs 10
