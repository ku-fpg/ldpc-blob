{-

This module gives a specialized signature to unboxed arrays.

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Haskell.ArraySig where

import Data.Array.Base (IArray(..),listArray)
import Data.Array.Unboxed (UArray,(!),Ix(..))

import Data.ListMatrix

type M = UArray (Int,Int)
type V = UArray Int

-- FYI: hackage says that UArray i Bool is packed at one bit per bool

fromListMatrix :: IArray UArray a => ListMatrix a -> M a
fromListMatrix (ListMatrix rows) =
 listArray ((1,1),(length rows,length (head rows))) $ concat rows

type family Mult_Result a b
class Mult a b where multE :: a -> b -> Mult_Result a b
class Add p c  where addE  :: c -> p -> c
class Zero c   where zeroE :: c

type instance Mult_Result Bool Bool = Bool
instance Mult Bool Bool where multE = (&&)
instance Add  Bool Bool where addE  = (/=)
instance Zero Bool      where zeroE = False

type instance Mult_Result Double Double = Double
instance Mult Double Double where multE = (*)
instance Add  Double Double where addE  = (+)
instance Zero Double        where zeroE = 0

{-# SPECIALISE multVM :: V Bool   -> M Bool   -> V Bool   #-}
{-# SPECIALISE multVM :: V Double -> M Double -> V Double #-}

-- | Multiply vector by a matrix.
multVM :: (IArray UArray a,IArray UArray b,IArray UArray c
          ,Mult a b,Add (Mult_Result a b) c,Zero c) =>
  V a -> M b -> V c
multVM v m
  | len/=numRow = error $ "Haskell.Encode.multVM: bad dimensions, " ++ show vBounds ++ " " ++ show mBounds
  | otherwise = listArray (cBase,cTop) $ go vBase rBase cBase zeroE where

  !len = rangeSize vBounds
  vBounds@(vBase,_) = bounds v
  !numRow = rangeSize (rBase,rTop)
--  !numCol = rangeSize (cBase,cTop)
  mBounds@((rBase,cBase),(rTop,cTop)) = bounds m

  -- iv, row, and col are the indices; acc accumulates the sum of the current
  -- column
  --
  -- (this is ripe for fusion with listArray -- I don't know to what extent
  -- that already happens)
  go !iv !row !col !acc
    | row==rTop = acc : if col==cTop then [] else go vBase rBase (col+1) zeroE
    | otherwise = let !o1 = v!iv
                      !o2 = m!(row,col)
                      !s  = multE o1 o2
                  in go (iv+1) (row+1) col (addE acc s)
