module Haskell where

import ECC
import Noise

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
  { generate = listArray (1,frameSize) <$> generateList gen frameSize
  , encode = return . encoder g
  , txRx = \v -> listArray (bounds v) <$> addNoise gen rate noisyness (elems v)
  , decode = return . decoder_mutation maxIterations h
  , check = \x y -> checkList (elems x) (elems y)
  , ber = berForFramesize frameSize
  , debug = liftIO . putStrLn
  , showX = \i -> Just $ "iterations " ++ show i
  , showV = show
  , showW = show
  , showWBool = show
  }
  where -- I copied this def from the href repo; not sure what "punc" stands
        -- for, but it looks like its the full width of the received data
        rate   = (fromIntegral packet_size)/(fromIntegral punc_size)

        numRows = rangeSize (rBase,rTop)
        numCols = rangeSize (cBase,cTop)
        ((rBase,cBase),(rTop,cTop)) = bounds h

        numParityBits = numRows
        frameSize = numCols-numParityBits

        packet_size = frameSize
        punc_size = numCols
