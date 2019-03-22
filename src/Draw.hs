{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Draw where

import Graphics.Gloss
import Battle
import Squad (Unit, Squad)
import Hex (Position)

windowWidth :: Int
windowWidth = 1280

windowHeight :: Int
windowHeight = 960

-- draw interface outside of battle
drawMenu :: Picture
drawMenu = undefined

-- draw single unit
drawUnit :: Unit -> Picture
drawUnit = undefined

-- draw single squad i.e. bunch of units
drawSquad :: Squad -> Picture
drawSquad = undefined

--function evenr_offset_to_pixel(hex):
--    var x = size * sqrt(3) * (hex.col - 0.5 * (hex.row&1))
--    var y = size * 3/2 * hex.row
--    return Point(x, y)

evenrOffsetToPixel :: Float -> Position -> Point
evenrOffsetToPixel size (col, row)= 
    let x = size * sqrt 3.0 * ((fromIntegral col) - 0.5 * (fromIntegral (mod row 2)))
        y = size * 3/2 * (fromIntegral row)
    in (x, y)

naturalOffset :: Point -> Point
naturalOffset (x,y) = (x - 300.0, y - 300.0)

hexCenter :: Float -> Position -> Point
hexCenter size pos = naturalOffset (evenrOffsetToPixel size pos)

hexPath :: Float -> Position -> [Point]
hexPath size pos =
    let center = hexCenter size pos
        x = fst center
        y = snd center
        topCorner = (x, y - size)
        topLeftCorner = (x - size * (sqrt 3.0) / 2, y - size / 2)
        topRightCorner = (x + size * (sqrt 3.0) / 2, y - size / 2)
        botCorner = (x, y + size)
        botLeftCorner = (x - size * (sqrt 3.0) / 2, y + size / 2)
        botRightCorner = (x + size * (sqrt 3.0) / 2, y + size / 2)
    in (topCorner:topRightCorner:botRightCorner:botCorner:botLeftCorner:topLeftCorner:[])

hexPath' :: Float -> Point -> [Point]
hexPath' size center =
    let x = fst center
        y = snd center
        topCorner = (x, y - size)
        topLeftCorner = (x - size * (sqrt 3.0) / 2, y - size / 2)
        topRightCorner = (x + size * (sqrt 3.0) / 2, y - size / 2)
        botCorner = (x, y + size)
        botLeftCorner = (x - size * (sqrt 3.0) / 2, y + size / 2)
        botRightCorner = (x + size * (sqrt 3.0) / 2, y + size / 2)
    in (topCorner:topRightCorner:botRightCorner:botCorner:botLeftCorner:topLeftCorner:[])

drawCell :: Float -> Cell -> Picture
drawCell size c = pictures ((polygon (hexPath size (position c))):(color white (polygon (hexPath' (size * (1 - hexStroke)) (hexCenter size (position c))))):[])

-- draw hexogonal grid
drawHexField :: Float -> HexField -> Picture
drawHexField size field = pictures (map (drawCell size) field)

hexSize :: Int -> Int -> Float
hexSize _ _ = hexConstSize
--hexSize = hexSizeFromWindowSize

hexSizeFromWindowSize :: Int -> Int -> Float
hexSizeFromWindowSize fieldHeight fieldWidth = 
    let maxHeight = 0.5 * ((fromIntegral windowHeight) / (fromIntegral fieldHeight))
        maxWidth  = sqrt (1/3) * 0.5 * ((fromIntegral windowWidth ) / (fromIntegral fieldWidth))
    in  min maxHeight maxWidth

hexConstSize :: Float
hexConstSize = 100.0

hexStroke :: Float
hexStroke = 0.02

hexSquadOffset :: Float
hexSquadOffset = 0.05

-- draw field, all terrain and all squads in it
drawBattleScene :: Battle -> Picture
drawBattleScene b = drawHexField (hexSize (fieldHeight b) (fieldWidth b)) (field b)

-- Game display mode.
window :: Display
window = InWindow "Game" (windowWidth, windowHeight) (10,10)

-- Background color.
bgColor :: Color
bgColor = greyN 0.3


drawGame :: Battle -> IO ()
drawGame b = display window bgColor (drawBattleScene b) 
