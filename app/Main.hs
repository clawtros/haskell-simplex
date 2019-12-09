module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [rate, scale] -> 
            animateSimplex (read rate :: Double) (read scale :: Double)
        _ ->
            animateSimplex 0.002 0.05