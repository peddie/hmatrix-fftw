{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Numeric/FFT/HMatrix/Matrix.hs"
               ]
