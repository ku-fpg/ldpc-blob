{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

module Codes.ICFP_Paper where

import Data.Sized.Fin
import Data.ListMatrix

import GHC.TypeLits

import System.IO.Unsafe (unsafePerformIO)

h_4096_7168 :: ListMatrix Bool
h_4096_7168 = expand h_4096_7168_compact

-- this g has a lot more ones in it than h does
g_4096_7168 :: ListMatrix Bool
g_4096_7168 = ListMatrix $ map (go 1) ones where
  ones :: [[Int]]
  ones = unsafePerformIO $ do
    x <- (map (map read . words) . lines) `fmap` readFile "Codes/G.ones"
    let go1 [] = ()
        go1 (x:xs) = x `seq` go1 xs
        go2 [] = ()
        go2 (x:xs) = go1 x `seq` go2 xs
    go2 x `seq` putStrLn "Done reading g"
    return x

  go i [] = replicate (7169 - i) False
  go i s@(one:ones) = hit : go (i+1) (if hit then ones else s)
    where hit = i == one

h_4096_7168_compact :: ListMatrix (Maybe (Fin 256))
h_4096_7168_compact = fmap cleanup $ listMatrix
  [ [ nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, 0  , nul, nul, nul, nul, nul, nul, nul, 0  , nul, nul, 160 ]
  , [ nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, 0  , nul, nul, nul, nul, nul, nul, 0  , 0  , nul, nul ]
  , [ nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, 0  , nul, nul, nul, nul, nul, nul, 0  , 0  , nul ]
  , [ nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, nul, 0  , nul, nul, nul, nul, nul, nul, 0  , 0   ]
  , [ 248, 12 , 111, nul, 0  , nul, nul, nul, 0  , nul, nul, nul, 0  , nul, nul, nul, nul, nul, nul, nul, 0  , nul, nul, nul, 241, 185, 251, nul ]
  , [ nul, 55 , 12 , 227, nul, 0  , nul, nul, nul, 0  , nul, nul, nul, 0  , nul, nul, nul, nul, nul, nul, nul, 0  , nul, nul, nul, 182, 249, 65  ]
  , [ 23 , nul, 147, 54 , nul, nul, 0  , nul, nul, nul, 0  , nul, nul, nul, 0  , nul, nul, nul, nul, nul, nul, nul, 0  , nul, 214, nul, 35 , 167 ]
  , [ 99 , 105, nul, 133, nul, nul, nul, 0  , nul, nul, nul, 0  , nul, nul, nul, 0  , nul, nul, nul, nul, nul, nul, nul, 0  , 7  , 31 , nul, 162 ]
  , [ 0  , nul, nul, nul, 66 , nul, 173, 42 , 0  , nul, nul, nul, nul, nul, 209, 103, nul, nul, nul, nul, 90 , 184, nul, nul, 0  , nul, nul, nul ]
  , [ nul, 0  , nul, nul, 243, 42 , nul, 52 , nul, 0  , nul, nul, 141, nul, nul, 70 , nul, nul, nul, nul, nul, 237, 77 , nul, nul, 0  , nul, nul ]
  , [ nul, nul, 0  , nul, 20 , 197, 93 , nul, nul, nul, 0  , nul, 84 , 206, nul, nul, nul, nul, nul, nul, nul, nul, 122, 67 , nul, nul, 0  , nul ]
  , [ nul, nul, nul, 0  , nul, 97 , 91 , 17 , nul, nul, nul, 0  , nul, 164, 11 , nul, nul, nul, nul, nul, 125, nul, nul, 237, nul, nul, nul, 0   ]
  ]
  where nul = negate 1
        cleanup :: Int -> Maybe (Fin 256)
        cleanup i | i < 0 = Nothing
                  | otherwise = Just $ toEnum $ fromEnum i

expand :: forall n. SingI n => ListMatrix (Maybe (Fin n)) -> ListMatrix Bool
expand = ListMatrix
       . concatMap (foldr (zipWith (++)) (replicate size []))
       . unListMatrix
       . fmap (unListMatrix . rotated) where

  size :: Int
  size = fromEnum $ fromNat (sing :: TNat n)

  rotated :: Maybe (Fin n) -> ListMatrix Bool
  rotated e = case e of
    Nothing -> zeroes
    Just fin -> case fromEnum fin of
      0 -> ones -- memoize a common case
      i -> rotateMatrix i (size,ones)

  zeroes,ones :: ListMatrix Bool
  zeroes = ListMatrix $ replicate size $ replicate size False
  ones = fmap (==1) $ identity size

rotateMatrix :: Int -> (Int,ListMatrix a) -> ListMatrix a
rotateMatrix by (width,ListMatrix rows) =
  ListMatrix $ map rotateList rows where
  rotateList :: [a] -> [a]
  rotateList l = let (a,b) = splitAt (width - (by `mod` width)) l in b ++ a




h_7_20 =fmap (/=0) $ listMatrix
  [ [1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
  , [1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
  , [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
  , [0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
  , [1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ]
  , [1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
  , [0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 ]
  , [0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ]
  , [0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]
  , [1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ]
  , [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ]
  , [0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 ]
  , [1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
  ]

g_7_20 = fmap (/=0) $ listMatrix
  [ [1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1]
  , [0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1]
  , [0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0]
  , [0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1]
  , [0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1]
  , [0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1]
  , [0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0]
  ]
