import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad.Random.Strict
import System.Random
import System.Random
import Data.Numbers.Primes

import Lib

main :: IO ()
main = hspec $ do
  describe "randomPrime" $ do
    it "generates prime numbers" $ do
      property $ \a ->
        let generator = mkStdGen a
            randomPrime' = randomPrime (1, 1000000000) Nothing
            prime = evalRand randomPrime' generator
        in isPrime prime

    it "does not give you the excluded value" $ do
      property $ \a ->
        let generator = mkStdGen a
            randomPrime' = randomPrime (1, 100)
            firstPrime = evalRand (randomPrime' Nothing) generator
            prime = evalRand (randomPrime' (Just firstPrime)) generator
        in isPrime prime && prime /= firstPrime

