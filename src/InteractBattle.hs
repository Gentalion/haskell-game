module InteractBattle where

import Graphics.Gloss.Interface.IO.Interact
import Battle
import Const
import Hex (Position)

pixelToEvenr :: Float -> Point -> Position
pixelToEvenr size (x, y) =
    let row = round (- 2/3 * y / size)
        col = round ((x + 0.5 * (fromIntegral (mod row 2)) * size * sqrt 3.0) / size / sqrt 3.0)
    in (col, row)

managePlayerInput :: Event -> Battle -> Battle
managePlayerInput e b = b