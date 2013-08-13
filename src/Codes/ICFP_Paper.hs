{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

module Codes.ICFP_Paper where

import Data.Sized.Fin
import Data.ListMatrix

import GHC.TypeLits

g_4096_7168 :: ListMatrix Bool
g_4096_7168 = expand g_4096_7168_compact

g_4096_7168_compact :: ListMatrix (Maybe (Fin 256))
g_4096_7168_compact = fmap cleanup $ listMatrix
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
