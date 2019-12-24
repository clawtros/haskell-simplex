module Main where

import Lib (animateSimplex)
import System.Environment
import System.Console.Terminal.Size

main :: IO ()
main = do
    args <- getArgs
    gotSize <- size
    (w, h) <- pure $
      case gotSize of
        Just (Window { height=height, width=width }) ->
          (width, height)
        Nothing ->
          (50, 20)

    case args of
        [rate, scale] -> 
            animateSimplex (read rate :: Double) (read scale :: Double) w (h + 1) 0
        _ ->
            animateSimplex 0.002 0.05 w h 0