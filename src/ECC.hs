-- Simple tester for LDPC-like things
module ECC where

import Data.Sized.Fin
import Data.Sized.Matrix
import qualified Data.Map as Map
import Data.Map (Map)

-- basic structure of an error-checking code
data ECC m v w = ECC
     { generate  ::                        m (v Bool)
     , encode    :: v Bool		-> m (w Bool)
     , txRx      :: w Bool		-> m (w Double)
     , decode    :: v Bool -> w Double 	-> m (v Bool)
     , check     :: v Bool -> v Bool    -> m Bool
     }

-- returns the number of successful runs
runECC :: ECC IO v w -> Int -> IO Int
runECC = undefined
