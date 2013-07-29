{-# LANGUAGE BangPatterns #-}
-- Simple tester for LDPC-lie things
module ECC where

import Data.Sized.Fin
import Data.Sized.Matrix
import qualified Data.Map as Map
import Data.Map (Map)
import System.Random.MWC
import Control.Monad.Primitive (PrimMonad, PrimState, unsafePrimToIO)

-- basic structure of an error-checking code
data ECC m v w = ECC
     { generate  ::                        m (v Bool)
     , debug     :: String              -> m ()
     , encode    :: v Bool		-> m (w Bool)
     , txRx      :: w Bool		-> m (w Double)
     , decode    :: v Bool -> w Double 	-> m (v Bool)
     , check     :: v Bool -> v Bool    -> m Integer
     }

-- returns the number of bits transmitted, and bits recieved intact.
runECC :: (Monad m) => ECC m v w -> Integer -> m Integer
runECC ecc count = run 0 0
  where
    run !n !errs
     | n == count = do
        debug ecc $ "returning " ++ show errs
        return errs
     | otherwise = do
        debug ecc $ "starting packet " ++ show n
        code0 <- generate ecc
        code1 <- encode ecc  code0
        rx    <- txRx ecc    code1
        code2 <- decode ecc  code0 rx
        bitErrors <- check ecc code0 code2
        run (n+1) (errs + bitErrors)

