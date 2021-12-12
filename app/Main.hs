{-# OPTIONS_GHC -Wall #-}

import Data.Char()

toDigits :: Integer -> [Integer]
toDigits x 
  | (== 0) x        = []
  | (< 0) x         = []
  | otherwise       = [x]

