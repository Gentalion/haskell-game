module Const where

import Graphics.Gloss (Point)
import Graphics.Gloss.Data.Color

bgColor :: Color
bgColor = greyN 0.3

hexConstSize :: Float
hexConstSize = 100.0

hexStroke :: Float
hexStroke = 0.03

squadOffset :: Float
squadOffset = 0.05

unitBetween :: Float
unitBetween = 0.1

windowWidth :: Int
windowWidth = 1280

windowHeight :: Int
windowHeight = 960

framesPerSecond :: Int
framesPerSecond = 60

-- how long rotation takes in seconds
rotationOn60DegreesAnimationTime :: Float
rotationOn60DegreesAnimationTime = 1.0

movementAnimationTime :: Float
movementAnimationTime = 0.5

hexMaximumInWindowSize :: Int -> Int -> Int -> Int -> Float
hexMaximumInWindowSize width height inRow inCol = 
    let hexesMaxWidth = fromIntegral width / ((sqrt 3.0) * ((fromIntegral inRow) + 0.5) + 1)
        hexesMaxHeight = fromIntegral height / (1.5 * fromIntegral inCol + 1)
    in min hexesMaxWidth hexesMaxHeight

naturalOffset :: (Float, Int, Int) -> Point -> Point
naturalOffset (size, width, height) (x,y) = (x - fromIntegral width / 2 + (sqrt 3.0) * (1 + hexStroke / 2) * size + size / 2, y + fromIntegral height / 2 - (1 + hexStroke) * size - size / 2)

maxDamageFromAttackerPower :: Float
maxDamageFromAttackerPower = 0.25

maxDamageFromDefenderPower :: Float
maxDamageFromDefenderPower = 0.1

pixelToEvenr :: (Float, Int, Int) -> (Float, Float) -> (Int, Int)
pixelToEvenr (size, width, height) (x1,y1) =
    let (x2,y2) = (naturalOffset (size, width, height) (0.0,0.0))
        (x,y) = (x1-x2,y1-y2)
        row = round (- 2/3 * y / size)
        col = round ((x + 0.5 * (fromIntegral (mod row 2)) * size * sqrt 3.0) / size / sqrt 3.0)
    in (col, row)