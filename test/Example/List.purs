module Example.List where

import Prelude
import Data.Array as A
import Data.List.Lazy as L
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.List.Trans (ListT, runListT)
import Control.MonadZero (guard)

-- based on http://hackage.haskell.org/package/list-transformer
logList :: forall eff.
   ListT (Eff (console :: CONSOLE | eff)) String
-> Eff (console :: CONSOLE | eff) Unit
logList l = runListT do
  liftEff $ log "logging listT"
  str <- l
  liftEff $ log str

-- based on https://wiki.haskell.org/ListT_done_right#Sum_of_squares
sumSqrs :: forall eff.
   Int
-> ListT (Eff (console :: CONSOLE | eff)) Unit
sumSqrs n = do
  let
    nats = L.iterate (add one) zero -- lazy infinite list
    squares = L.toUnfoldable <<< L.takeWhile (_ <= n) $ map (\x -> x * x) nats
  x <- squares
  y <- squares
  liftEff $ log ("<" <> show x <> "," <> show y <> ">")
  guard $ x + y == n
  liftEff $ log "Sum of squares."

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  logList $ A.toUnfoldable ["one", "two", "three"]
  runListT $ sumSqrs 10
