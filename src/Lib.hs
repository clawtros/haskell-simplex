module Lib
  ( animateSimplex
  )
where

import Simplex
import Data.List
import Data.Maybe
import Control.Parallel.Strategies (parList, rdeepseq, using)

nth :: [a] -> Int -> Maybe a
nth l n =
  case drop n l of
    [] -> Nothing
    a:rest -> Just a

shade :: Double -> String
shade n =
  let chars =
        " _.,~=|iI]Z3VGdDQg#@"

  in [fromMaybe '?' $ nth chars (floor $ (n + 0.5) * (fromIntegral $ length chars))]

animateSimplex :: Double -> Double -> Int -> Int -> Double -> IO ()
animateSimplex rate scale w h z =
  let grid = [[ (fromIntegral x :: Double, fromIntegral y :: Double)
        | y <- [1 .. w] ]
        | x <- [1 .. (h - 1)] ]

      rowF = (\row -> concat $ intersperse "" $ map
          (\(x_, y_) -> shade (noise3D (x_ * scale) (y_ * scale) z))
          row
        )

      mapped = (map
                rowF
                grid
               ) `using` parList rdeepseq
      
      strgrid = concat $ intersperse "\n" mapped
  in do
        putStr "\ESC[1;1H"
        putStr strgrid
        animateSimplex rate scale w h (z + rate)
                            
  