{-# LANGUAGE DuplicateRecordFields #-}

module Lib where

import Control.Monad.Random.Strict (Rand, RandomGen, getRandomR)
import Data.Word (Word8)
import Data.Bits ((.|.))
import Data.Numbers.Primes (isPrime)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU

data PublicKey = PublicKey
  { n :: Integer
  , e :: Integer
  } deriving (Show)

data PrivateKey = PrivateKey
  { n :: Integer
  , d :: Integer
  } deriving (Show)

os2ip :: BS.ByteString -> Integer
os2ip = BS.foldl' (\a b -> (a * 256) .|. (fromIntegral b)) 0

i2osp :: Integer -> BS.ByteString
i2osp =
  let unfold 0 = Nothing
      unfold v = Just (fromIntegral $ rem v 256, div v 256)
  in BS.reverse . BS.unfoldr unfold

randomPrime :: (RandomGen g)
            => (Integer, Integer)
            -> Maybe Integer
            -> Rand g Integer
randomPrime range exclude = do
  r <- getRandomR range
  if isPrime (r::Integer) && Just r /= exclude
    then pure r
    else randomPrime range exclude

generateKeypair :: (RandomGen g) => Rand g (PublicKey, PrivateKey)
generateKeypair = do
  p <- randomPrime (1000000000000, 9999999999999) Nothing
  q <- randomPrime (1000000000000, 9999999999999) Nothing
  let n = p * q
  let phi_n = (p-1) * (q-1)
  let e = 2^16 + 1
  let k = 2
  let d = (k * phi_n + 1) `div` e
  pure (PublicKey n e, PrivateKey n d)

encrypt :: PublicKey -> BS.ByteString -> Integer
encrypt (PublicKey n e) plaintext =
  let m = os2ip plaintext
      c = m^e `mod` n
  in c

decrypt :: PrivateKey -> Integer -> BS.ByteString
decrypt (PrivateKey n d) c =
  let m = c^d `mod` n
  in i2osp m

blah :: RandomGen g => Rand g Int
blah = getRandomR (1, 6)