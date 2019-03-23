module InteractBattle where

import Graphics.Gloss
import Battle
import Const
import Hex (Position)

--evenrToPixel :: Float -> Position -> Point
--evenrToPixel size (col, row) = 
--    let x = size * sqrt 3.0 * ((fromIntegral col) - 0.5 * (fromIntegral (mod row 2)))
--        y = - size * 3/2 * (fromIntegral row)
--    in (x, y)

pixelToEvenr :: Float -> Point -> Position
pixelToEvenr size (x, y) =
    let row = round (- 2/3 * y / size)
        col = round ((x + 0.5 * (fromIntegral (mod row 2)) * size * sqrt 3.0) / size / sqrt 3.0)
    in (col, row)