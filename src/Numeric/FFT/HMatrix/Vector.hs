{-# OPTIONS_GHC -Wall #-}

module Numeric.FFT.HMatrix.Vector (
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
import Numeric.LinearAlgebra.Data ( Vector, Complex )
import Numeric.LinearAlgebra ( Element )
import qualified Numeric.LinearAlgebra.Data as M
import qualified Numeric.FFT.Vector.Unnormalized as FFT

dft :: Vector (Complex Double) -> Vector (Complex Double)
dft = FFT.run FFT.dft

idft :: Vector (Complex Double) -> Vector (Complex Double)
idft = FFT.run FFT.idft

dftR2C :: Vector Double -> Vector (Complex Double)
dftR2C = FFT.run FFT.dftR2C

dftC2R :: Vector (Complex Double) -> Vector Double
dftC2R = FFT.run FFT.dftC2R

dct1 :: Vector Double -> Vector Double
dct1 = FFT.run FFT.dct1

dct2 :: Vector Double -> Vector Double
dct2 = FFT.run FFT.dct2

dct3 :: Vector Double -> Vector Double
dct3 = FFT.run FFT.dct3

dct4 :: Vector Double -> Vector Double
dct4 = FFT.run FFT.dct4

dst1 :: Vector Double -> Vector Double
dst1 = FFT.run FFT.dst1

dst2 :: Vector Double -> Vector Double
dst2 = FFT.run FFT.dst2

dst3 :: Vector Double -> Vector Double
dst3 = FFT.run FFT.dst3

dst4 :: Vector Double -> Vector Double
dst4 = FFT.run FFT.dst4
