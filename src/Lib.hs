module Lib
    ( animateSimplex
    ) where

import Simplex
import Data.List


clear :: IO ()
clear = putStr "\ESC[2J"

shade :: Double -> String
shade n =
  let chars = " .\9617\9618\9619\9608"
  in
  [chars !! (floor $ (n + 0.5) * (fromIntegral $ length chars))]

animateSimplex :: Double -> Double -> IO ()
animateSimplex rate scale =
  let
    grid = [[(x, y) | y <- [1..160]] | x <- [1..50]]
    strgrid z = intercalate "\n"
      $ map (\row ->
                concatMap (\(x_, y_) ->
                          shade (noise3D (x_ * scale) (y_ * scale) z)) row
         ) grid
  in
    sequence_
    $ concatMap (\n -> [
              putStr "\ESC[1;1H"
              , putStrLn
                $ strgrid
                $ n * rate
              ]) [1..100000]
  