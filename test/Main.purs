module Test.Main where

import Control.Bind (discard)
import Control.Coercible (class Coercible, coerce)
import Data.Function (($))
import Data.List ((:), List(Nil))
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (class Show, show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (logShow)


data N = Z | S N

instance showN :: Show N where
  show Z = "Z"
  show (S n) = "S" <> show n

instance coerceNInt :: Coercible N Int where
  coerce Z = 0
  coerce (S n) = 1 + coerce n
else 
instance coerceNString :: Coercible N String where
  coerce = show

main :: Effect Unit
main = do
  let three = S (S (S Z))
  logShow $ (coerce three :: String)
  logShow $ (coerce three :: Int)
  logShow $ (coerce 3 :: Number)
  logShow $ (coerce '3' :: String)
  logShow $ (coerce 3 :: Unit)
  logShow $ (coerce 3 :: Array Int)
  logShow $ (coerce (coerce 3 :: Unit) :: Array Unit)
  logShow $ (coerce ('a' : 'b' : 'c' : Nil) :: String)
  logShow $ (coerce ['a', 'b', 'c'] :: String)
