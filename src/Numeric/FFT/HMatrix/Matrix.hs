{-# OPTIONS_GHC -Wall #-}

module Numeric.FFT.HMatrix.Matrix (
  -- * Complex-to-complex transforms
  dft
  , idft
    -- * Real-to-complex transforms
  , dftR2C
  , dftC2R
    -- * Real-to-real transforms
    -- ** Discrete cosine transforms
  , dct1
  , dct2
  , dct3
  , dct4
    -- ** Discrete sine transforms
  , dst1
  , dst2
  , dst3
  , dst4
  ) where

import Foreign.Storable ( Storable )
import Numeric.LinearAlgebra.Data ( Matrix, Complex )
import Numeric.LinearAlgebra ( Element )
import qualified Numeric.LinearAlgebra.Data as M
import Numeric.FFT.Vector.Unnormalized ( Transform )
import qualified Numeric.FFT.Vector.Unnormalized as FFT

transResize :: (Storable a, Element b) =>
               (Matrix b -> Int)
            -> ((Int, Int) -> Transform b a)
            -> Matrix b
            -> Matrix a
transResize calcCols f m = M.reshape newcols . FFT.run (f (r, c)) $ M.flatten m
  where
    c = M.cols m
    r = M.rows m
    newcols = calcCols m

trans :: (Storable a, Element b) =>
         ((Int, Int) -> Transform b a)
      -> Matrix b
      -> Matrix a
trans = transResize (M.cols)

-- |
-- >>> M.toLists . dft $ M.fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- [[45.0 :+ 0.0,(-4.5) :+ 2.598076211353316,(-4.5) :+ (-2.598076211353316)],[(-13.5) :+ 7.794228634059947,0.0 :+ 0.0,0.0 :+ 0.0],[(-13.5) :+ (-7.794228634059947),0.0 :+ 0.0,0.0 :+ 0.0]]
dft :: Matrix (Complex Double) -> Matrix (Complex Double)
dft = trans FFT.dft2d

-- |
-- >>> M.toLists . idft $ M.fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- [[45.0 :+ 0.0,(-4.5) :+ (-2.598076211353316),(-4.5) :+ 2.598076211353316],[(-13.5) :+ (-7.794228634059947),0.0 :+ 0.0,0.0 :+ 0.0],[(-13.5) :+ 7.794228634059947,0.0 :+ 0.0,0.0 :+ 0.0]]
idft :: Matrix (Complex Double) -> Matrix (Complex Double)
idft = trans FFT.idft2d

-- |
-- >>> M.toLists . dftR2C $ M.fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- [[45.0 :+ 0.0,(-4.5) :+ 2.598076211353316],[(-13.5) :+ 7.794228634059947,0.0 :+ 0.0],[(-13.5) :+ (-7.794228634059947),0.0 :+ 0.0]]
dftR2C :: Matrix Double -> Matrix (Complex Double)
dftR2C = transResize (\m -> M.cols m `div` 2 + 1) FFT.dftR2C2D

-- |
--
-- >>> let m = M.fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- >>> let mtest = M.fromLists [[33,-3,-3,-9],[3,-3,-9,-3],[3,0,0,0]]
-- >>> M.maxElement (mtest - dftC2R m) < 1e-12
-- True
dftC2R :: Matrix (Complex Double) -> Matrix Double
dftC2R = transResize (\m -> 2 * (M.cols m - 1)) FFT.dftC2R2D

dct1 :: Matrix Double -> Matrix Double
dct1 = trans FFT.dct1_2D

dct2 :: Matrix Double -> Matrix Double
dct2 = trans FFT.dct2_2D

dct3 :: Matrix Double -> Matrix Double
dct3 = trans FFT.dct3_2D

dct4 :: Matrix Double -> Matrix Double
dct4 = trans FFT.dct4_2D

dst1 :: Matrix Double -> Matrix Double
dst1 = trans FFT.dst1_2D

dst2 :: Matrix Double -> Matrix Double
dst2 = trans FFT.dst2_2D

dst3 :: Matrix Double -> Matrix Double
dst3 = trans FFT.dst3_2D

dst4 :: Matrix Double -> Matrix Double
dst4 = trans FFT.dst4_2D
