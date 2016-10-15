module Cipher where

import Data.Char

caesar :: String -> Int -> String
caesar [] _ = []
caesar (x : xs) k = (cipher x k) : caesar xs k
  where cipher x k = chr $ ord 'a' + mod (ord x - ord 'a' + k) 26

unCaesar :: String -> Int -> String
unCaesar xs k = caesar xs (-k)

