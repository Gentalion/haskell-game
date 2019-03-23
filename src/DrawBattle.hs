{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DrawBattle where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss
import Battle
import Squad
import Const
import Hex (Position)
import InteractBattle

colorSquad :: Squad -> Color
colorSquad squad | (control squad) == Player = blue
                 | (control squad) == EnemyAI = red
                 | otherwise = black

(+++) :: Point -> Point -> Point
(+++) pos1 pos2 = (fst pos1 + fst pos2, snd pos1 + snd pos2) 

translate' :: Point -> Picture -> Picture
translate' pos pic = translate (fst pos) (snd pos) pic

-- draw single squad i.e. bunch of units
drawSquad :: Float -> Point -> Squad -> Picture
drawSquad hexSize pos squad = 
    let unitSize = (hexSize * (1 - 2 * (hexStroke + squadOffset + unitBetween))) / 6
        unitOffset = unitSize * 2 + unitBetween * hexSize
        posX = fst pos
        posY = snd pos
        unitsNum = length (units squad)
    in pictures (take unitsNum [color (colorSquad squad) 
              $ translate' (pos +++ (rotateV (degToRad (rotation squad)) (offsetX, offsetY))) 
              $ circleSolid unitSize | offsetX <- [0, -unitOffset, unitOffset], offsetY <- [0, unitOffset, -unitOffset]])

evenrToPixel :: Float -> Position -> Point
evenrToPixel size (col, row) = 
    let x = size * sqrt 3.0 * ((fromIntegral col) - 0.5 * (fromIntegral (mod row 2)))
        y = - size * 3/2 * (fromIntegral row)
    in (x, y)

hexCenter :: Float -> Position -> Point
hexCenter size pos = naturalOffset (evenrToPixel size pos)

hexPath :: Float -> Point -> [Point]
hexPath size center =
    let x = fst center
        y = snd center
        topCorner = (x, y - size)
        topLeftCorner = (x - size * (sqrt 3.0) / 2, y - size / 2)
        topRightCorner = (x + size * (sqrt 3.0) / 2, y - size / 2)
        botCorner = (x, y + size)
        botLeftCorner = (x - size * (sqrt 3.0) / 2, y + size / 2)
        botRightCorner = (x + size * (sqrt 3.0) / 2, y + size / 2)
    in (topCorner:topRightCorner:botRightCorner:botCorner:botLeftCorner:topLeftCorner:[])

drawCell :: Float -> Color -> Cell -> Picture
drawCell size cellColor cell =
    let center = hexCenter size (position cell)
    in case (squad cell) of
        (Just squad) -> pictures ((polygon (hexPath (size * (1 + hexStroke)) center))
                                 :(color (mixColors 0.5 0.5 cellColor white) $ polygon (hexPath (size * (1 - hexStroke)) center))
                                 :(drawSquad size center squad)
                                 :[])
        (         _) -> pictures ((polygon (hexPath (size * (1 + hexStroke)) center))
                                 :(color (mixColors 0.5 0.5 cellColor white) $ polygon (hexPath (size * (1 - hexStroke)) center))
                                 :[])

-- draw hexogonal grid
drawHexField :: Float -> Color -> [Cell] -> Picture
drawHexField size color field = pictures (map (drawCell size color) field)

-- draw field, all terrain and all squads in it
drawBattleScene :: Battle -> Picture
drawBattleScene b = pictures ((drawHexField hexConstSize green $ sortByPositions (possibleMoves b) (field b))
                             :(drawHexField hexConstSize white $ otherByPositions (possibleMoves b) (field b))
                             :[])

-- Game display mode.
window :: Display
window = InWindow "Game" (windowWidth, windowHeight) (10,10)

-- Background color.
bgColor :: Color
bgColor = greyN 0.3


drawGame :: Battle -> IO ()
drawGame b = display window bgColor (drawBattleScene b) 
