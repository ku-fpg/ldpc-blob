module Haskell.Encode where

import Data.ListMatrixa

encoder :: Array (Int,Int) Bool -> Array Int Bool -> Array Int Bool

encoder :: ListMatrix Bool -> [Bool] -> [Bool]
encoder g v = w
  where
     ListMatrix [w] = listMatrix [v] `boolMM` g
