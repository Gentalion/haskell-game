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

naturalOffset :: Point -> Point
naturalOffset (x,y) = (x - 400.0, y + 250.0)