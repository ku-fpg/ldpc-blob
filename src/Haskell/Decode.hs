{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Haskell.Decode where

import Haskell.ArraySig

import Data.Array.Base (IArray(..),listArray,amap,elems)
import Data.Array.Base (thaw,unsafeFreeze)
import Data.Array.Unboxed ((!),Ix(..))
import Data.Array.ST (STUArray,readArray,writeArray)
import Control.Monad.ST (ST,runST)
import Control.Monad (when)
import Control.Applicative ((<$>),(<*>))

--import Control.Monad.ST.Unsafe (unsafeIOToST)

type STM s = STUArray s (Int,Int)
type STV s = STUArray s Int

decoder_mutation :: Int -> M Bool -> V Double -> (Int,V Bool)
decoder_mutation maxIterations h lam0
  | len /= numCol = error "Haskell.Encode.decoder: bad dimensions"
  | otherwise = runST $ do
    lam <- thaw lam0
    eta <- thaw (amap (const 0) h)
    debug $ forEtaRow $ \row -> do
      putStr "eta cols "
      forEtaCol row $ \col -> putStr (show col) >> putStr " "
      putStrLn ""
    n <- go 0 lam eta
    -- unsafeFreeze is safe because lam dies
    (,) n . amap (>0) . trim <$> unsafeFreeze lam where

  !len = rangeSize lamBounds
  lamBounds@(lamBase,_) = bounds lam0
  !numCol = rangeSize (cBase,cTop)
  !numRow = rangeSize (rBase,rTop)
  hBounds@((rBase,cBase),(rTop,cTop)) = bounds h

  trim :: V Double -> V Double
  trim = listArray (1,numCol-numRow) . elems

  forEta :: Monad m => (Int -> Int -> m ()) -> m ()
  forEta f = mapM_ (\idx -> when (h!idx) $ uncurry f idx) $ range hBounds

  forEtaRow :: Monad m => (Int -> m ()) -> m ()
  forEtaRow f = mapM_ f $ range (rBase,rTop)

  forEtaCol :: Monad m => Int -> (Int -> m ()) -> m ()
  forEtaCol row f = mapM_ (\col -> when (h!(row,col)) $ f col) $ range (cBase,cTop)

  forEtaCol' :: Monad m => Int -> (Int -> Bool -> m ()) -> m ()
  forEtaCol' row f = mapM_ (\col -> f col (h!(row,col))) $ range (cBase,cTop)

  foldlEtaCol :: Monad m => acc -> Int -> (acc -> Int -> m acc) -> m acc
  foldlEtaCol z row f = go (range (cBase,cTop)) z where
    go !cols !acc = case cols of
      [] -> return acc
      (col:cols) -> (if h!(row,col) then f acc col else return acc) >>= go cols

--  debug = unsafeIOToST -- switch to the bottom one to enable debugging
  debug :: IO () -> ST s ()
  debug = const (return ())

  forLamCol :: Monad m => (Int -> m ()) -> m ()
  forLamCol f = mapM_ f (range lamBounds)

  rnd :: Double -> Double
  rnd d = fromIntegral (round (d * 1000.0) :: Int) / 1000.0
  putStr7 s = putStr $ if len < 7 then replicate (7 - len) ' ' ++ s else s
    where len = length s

  go :: Int -> STV s Double -> STM s Double -> ST s Int
  go !n !lam !eta = if n >= maxIterations then return n else do
    debug $ putStrLn "---"

    forEtaRow $ \row -> do
      debug $ putStr "eta "
      forEtaCol' row $ \col enabled ->
        if not enabled then debug $ putStr7 ""
        else readArray eta (row,col) >>= \x -> debug $ putStr7 (show (rnd x)) >> putStr " "
      debug $ putStrLn ""
    debug $ putStr "lam "
    forLamCol $ \col -> readArray lam col >>= \x -> debug $ putStr (show x) >> putStr " "
    debug $ putStrLn ""

    -- eta[r,c] := eta[r,c] - lam[c]
    forEta $ \row col -> (>>= writeArray eta (row,col)) $
                         (-) <$> readArray eta (row,col) <*> readArray lam (col-cBase+lamBase)

    -- lam[c] := lam0[c]   -- is there a bulk operation for this?
    forLamCol $ \col -> writeArray lam col $ lam0!col

    forEtaRow $ \row -> do
      -- this is the clever way: take the whole row's min_dagger, then tweak it
      -- at each element

      -- collect the minimum and the next-most minimum in the whole row
      (sign,TwoMD the_min the_2nd_min) <- do
        let snoc (sign,md) x = (sign * signum x,minMD md $ abs x)
        foldlEtaCol (1,ZeroMD) row $ \mins col -> snoc mins <$> readArray eta (row,col)

      debug $ print (sign,the_min,the_2nd_min)

      -- eta[r,c] := min_dagger(eta[r,forall d. d /= c])
      -- lam[c]   := lam[c] + sum(eta[forall r. r,c])
      forEtaCol row $ \col -> do
        etav <- readArray eta (row,col)
        let etav' = negate $ (0.75*) $ sign * signum etav *
                    if abs etav == the_min -- dubious use of (==) Double
                    then the_2nd_min else the_min
        writeArray eta (row,col) etav'

{-    -- this, on the other hand, is the straight-forward way. (We might
      -- optimize to add the_mins array as a loop argument)

      the_mins <- newArray (cBase,cTop) 0

      forEtaCol row $ \col -> do
        the_min <- foldlEtaCol 2 row $ \the_min col2 -> do
          if col == col2 then return the_min
            else min_dagger the_min <$> readArray eta (row,col2)
        writeArray (the_mins `asTypeOf` lam) col the_min

      forEtaCol row $ \col -> do
        the_min <- readArray the_mins col
        let etav' = negate $ 0.75 * the_min
        writeArray eta (row,col) etav'
-}



        -- add the new eta value to lam
        lamv <- readArray lam (col-cBase+lamBase)
        let lamv' = lamv+etav'
        writeArray lam (col-cBase+lamBase) lamv'

      debug $ putStr "eta' "
      forEtaCol' row $ \col enabled ->
        if not enabled then debug $ putStr7 ""
        else readArray eta (row,col) >>= \x -> debug $ putStr7 (show (rnd x)) >> putStr " "
      debug $ putStrLn ""

    -- unsafeFreeze is safe because lam' doesn't survive this iteration
    parity <- unsafeFreeze lam >>= \lam' -> do
      let cHat = amap (>0) lam'
      let x = multVM cHat (transpose h)
      debug $ putStr "cHat " >> print (elems cHat)
      debug $ putStr "x    " >> print (elems x)
      debug $ putStr "lam  " >> print (elems $ lam' `asTypeOf` lam0)
      return $ x == listArray (rBase,rTop) (repeat False)
    if parity then return n else go (n+1) lam eta

{-# INLINE min_dagger #-}
min_dagger x y = signum x * signum y * min (abs x) (abs y)

data MD a = ZeroMD | OneMD a | TwoMD a a

{-# INLINE minMD #-}
minMD ZeroMD x = OneMD x
minMD (OneMD a) x
  | x < a = TwoMD x a
  | otherwise = TwoMD a x
minMD (TwoMD a b) x
  | x < a = TwoMD x a
  | x < b = TwoMD a x
  | otherwise = TwoMD a b
