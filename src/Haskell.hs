module Haskell where

import ECC

import Haskell.ArraySig
import Data.Array.Base (listArray,amap,elems)
import Haskell.Encode (encoder)
import Haskell.Decode (decoder_mutation)

import System.Random.MWC (create)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans (MonadIO(..))

import Control.Applicative ((<$>))

ecc_mutation :: (Functor m,PrimMonad m,MonadIO m) =>
  Int -> M Bool -> M Bool -> Int -> m (ECC m V V Double)
ecc_mutation maxIterations h g frameSize = create >>= \gen -> return $ ECC
  { generate = listArray (1::Int,frameSize) <$> generateList gen frameSize
  , encode = return . encoder g
  , txRx = return . amap (\ x -> if x then 1 else -1)
  , decode = return . decoder_mutation maxIterations h
  , check = \x y -> checkList (elems x) (elems y)
  , ber = berForFramesize frameSize
  , debug = liftIO . putStrLn
  , showV = show
  , showW = show
  , showWBool = show
  }
