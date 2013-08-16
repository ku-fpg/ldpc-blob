module Haskell where

import ECC
import Noise

import Codes.ICFP_Paper

import Haskell.ArraySig
import Data.Array.Base (listArray,elems,bounds)
import Data.Array (rangeSize)
import Haskell.Encode (encoder)
import Haskell.Decode (decoder_mutation)

import System.Random.MWC (create)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans (MonadIO(..))

import Control.Applicative ((<$>))

ecc_mutation :: (Functor m,PrimMonad m,MonadIO m) =>
  Int -> M Bool -> M Bool -> Noisyness -> m (ECC m V V Double Int)
ecc_mutation maxIterations h g noisyness =
  create >>= \gen -> return $ ECC
  { generate = listArray (1,originalBits) <$> generateList gen originalBits
  , encode = return . encoder g
    -- do not tx all of the parity bits
  , txRx = fmap (listArray (1,allBits))
         . fmap (++ replicate untxBits 0.0)
         . addNoise gen rate noisyness
         . take frameSize
         . elems
  , decode = return . decoder_mutation maxIterations h
  , check = \x y -> checkList (elems x) (elems y)
  , ber = berForFramesize frameSize
  , debug = liftIO . putStrLn
  , showX = \i -> Just $ "iterations " ++ show i
  , showV = map (\b -> if b then '1' else '0') . elems
  , showW = show . elems
  , showWBool = map (\b -> if b then '1' else '0') . elems
  }
  where -- I copied this def from the hfec repo; not sure what "punc" stands
        -- for, but it looks like it's the full width of the received data
        rate = (fromIntegral packet_size)/(fromIntegral punc_size)

        numRowsG = rangeSize (rBaseG,rTopG)
        numColsG = rangeSize (cBaseG,cTopG)
        ((rBaseG,cBaseG),(rTopG,cTopG)) = bounds g

        numRowsH = rangeSize (rBaseH,rTopH)
        ((rBaseH,_),(rTopH,_)) = bounds h

        -- the bits being encoded
        originalBits = numRowsG
        parityBits   = numColsG-originalBits
        allBits = originalBits+parityBits

        untxBits = numRowsG-numRowsH

        -- the number of bits transmitted
        frameSize = numColsG-untxBits

        packet_size = originalBits
        punc_size = numColsG

main :: IO ()
main = mainWith $ ecc_mutation 200 (fromListMatrix Codes.ICFP_Paper.h_4096_7168) (fromListMatrix Codes.ICFP_Paper.g_4096_7168) Nothing
