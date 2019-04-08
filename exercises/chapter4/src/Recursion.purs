module Recursion where

import Prelude

import Data.Array
import Data.Maybe
import Data.Tuple

import Control.MonadZero (guard)

isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

recIsEven :: Int -> Boolean
recIsEven 2 = true
recIsEven 1 = false
recIsEven n = recIsEven (n - 2)

filterEven :: Array Int -> Array Int
filterEven arr = tailRec [] arr
  where
    tailRec :: Array Int -> Array Int -> Array Int
    tailRec acc rem = case uncons rem of
      Just { head, tail } | isEven head -> tailRec (snoc acc head) tail
      Just { head, tail } -> tailRec acc tail
      Nothing -> acc

squares :: Array Int -> Array Int
squares = map \x -> x * x

filterNegative :: Array Int -> Array Int
filterNegative = filter \x -> x >= 0

infixl 1 filter as <$?>

nFilterNegative :: Array Int -> Array Int
nFilterNegative arr = (\x -> x >= 0) <$?> arr

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 1

cartesianProduct :: forall a b. Array a -> Array b -> Array (Tuple a b)
cartesianProduct arr1 arr2 = do
  elem1 <- arr1
  elem2 <- arr2
  pure (Tuple elem1 elem2)

-- Construction taken from https://www.mathsisfun.com/numbers/pythagorean-triples.html
triples :: Int -> Array (Array Int)
triples n = do
  p <- 1 .. n
  q <- (p + 1) .. n
  let a = (q * q) - (p * p)
  let b = 2 * p * q
  let c = (p * p) + (q * q)
  guard ((a < n) && (b < n) && (c < n))
  pure [a, b, c]

factorizations :: Int -> Array (Array Int)
factorizations 1 = [[]]
factorizations n | isPrime n = [[n]]
factorizations n = do
  factor <- 2 .. (n / 2)
  guard $ (n `mod` factor) == 0
  otherFactors <- factorizations $ n / factor
  pure(factor : otherFactors)