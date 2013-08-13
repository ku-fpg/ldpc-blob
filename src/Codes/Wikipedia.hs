module Codes.Wikipedia where

--import Data.ListMatrix

-- THEY DO NOT WORK, or at least not well. I suspect I just lost two hours
-- because I didn't check that sooner...

{-
-- From http://en.wikipedia.org/wiki/Low-density_parity-check_code
-- This is a (6, 3) linear code, with n = 6 and k = 3.
h_6_3 :: ListMatrix Bool
h_6_3 = fmap (/= 0) $ listMatrix
  [ [ 1, 1, 1, 1, 0, 0 ]
  , [ 0, 0, 1, 1, 0, 1 ]
  , [ 1, 0, 0, 1, 1, 0 ]
  ]

g_6_3 :: ListMatrix Bool
g_6_3 = fmap (/= 0) $ listMatrix
  [ [ 1, 0, 0, 1, 0, 1 ]
  , [ 0, 1, 0, 1, 1, 1 ]
  , [ 0, 0, 1, 1, 1, 0 ]
  ]
-}

