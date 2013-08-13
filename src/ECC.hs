{-# LANGUAGE BangPatterns #-}
-- Simple tester for LDPC-lie things
module ECC where

import Data.Sized.Fin
import qualified Data.Map as Map
import Data.Map (Map)
import System.Random.MWC
import Control.Monad.Primitive (PrimMonad, PrimState, unsafePrimToIO)

-- basic structure of an error-checking code
data ECC m v w d = ECC
     { generate  ::                        m (v Bool)
     , encode    :: v Bool		-> m (w Bool)
     , txRx      :: w Bool		-> m (w d)
     , decode    :: v d 	        -> m (v Bool)
     , check     :: v Bool -> v Bool    -> m Integer
     , ber       :: Integer -> Integer  -> Double               -- ^ given this many frames, and this number of bit errors, what is the BER?
     , debug     :: String              -> m ()
     }

-- returns the number of bits transmitted, and bits recieved intact.
runECC :: (Monad m) => ECC m v w d -> Integer -> m Integer
runECC ecc count = run 0 0
  where
    run !n !errs
     | n == count = do
        debug ecc $ "returning " ++ show errs
        return errs
     | otherwise = do
        debug ecc $ "starting packet " ++ show n
        code0     <- generate ecc
        code1     <- encode ecc code0
        rx        <- txRx   ecc code1
        code2     <- decode ecc rx
        bitErrors <- check  ecc code0 code2
        run (n+1) (errs + bitErrors)


-- Utilties for building the ECC
generateList :: (PrimMonad m) => Gen (PrimState m) -> Integer -> m [Bool]
generateList gen sz = sequence [ uniform gen | _ <- [1..sz]]

encodeId :: (Monad m) => v Bool -> m (v Bool)
encodeId = return

decodeId :: (Monad m, Functor v) => v Double -> m (v Bool)
decodeId = return . fmap (>= 0)

checkList :: (Monad m) => [Bool] -> [Bool] -> m Integer
checkList xs ys
  | length xs /= length ys = error "internal error in checkList"
  | otherwise = return $ sum $ zipWith (\ x y -> if x == y then 0 else 1) xs ys

txRxId :: (Monad m, Functor w) => w Bool -> m (w Double)
txRxId = return . fmap (\ x -> if x then 1 else -1)

berForFramesize :: Integer -> (Integer -> Integer -> Double)
berForFramesize frameSize frames bitErrors = fromIntegral bitErrors / fromIntegral (frameSize * frames)

main = do
        let frameSize = 32
        let frames = 100
        gen <- create
        let ecc = ECC
                { generate = generateList gen frameSize
                , encode = encodeId
                , txRx   = txRxId
                , decode = decodeId
                , check = checkList
                , ber = berForFramesize frameSize
                , debug = putStrLn
                }
        errs <- runECC ecc frames
        print $ "BER = " ++ show (ber ecc frames errs)


