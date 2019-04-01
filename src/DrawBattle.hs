module DrawBattle where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss
import Battle
import Squad
import Const
import Hex (Position, evenrToPixel)
import InteractBattle
import Data.Default
import qualified MovingSquad

type MixColor = (Color, Float)

colorSquad :: Squad -> Color
colorSquad squad | (control squad) == Player = blue
                 | (control squad) == EnemyAI = red
                 | otherwise = black

colorSelection :: Battle -> Maybe Cell -> Color
colorSelection b Nothing = black
colorSelection b (Just cell) = case (control $ maybe def id $ squad cell) of 
    (EnemyAI) -> red
    ( Player) -> blue
    (      _) -> greyN 0.5

colorCellByTerrain :: Cell -> Color
colorCellByTerrain cell | (terrain cell) == TerPlain = mixColors 0.5 0.5 white yellow
                  | (terrain cell) == TerWater = blue
                  | otherwise = black

(+++) :: Point -> Point -> Point
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

translate' :: Point -> Picture -> Picture
translate' pos pic = translate (fst pos) (snd pos) pic

-- draw single squad i.e. bunch of units
drawSquad' :: Float -> Point -> Float -> Squad -> Picture
drawSquad' hexSize pos rotation squad = 
    let unitSize = (hexSize * (1 - 2 * (hexStroke + squadOffset + unitBetween))) / 6
        unitOffset = unitSize * 2 + unitBetween * hexSize
        posX = fst pos
        posY = snd pos
        unitsNum = length (units squad)
    in pictures (take unitsNum [color (colorSquad squad) 
              $ translate' (pos +++ (rotateV (degToRad rotation) (offsetX, offsetY))) 
              $ circleSolid unitSize | offsetX <- [0, -unitOffset, unitOffset], offsetY <- [0, unitOffset, -unitOffset]])

drawSquad :: Float -> Point -> Squad -> Picture
drawSquad hexSize pos squad =  drawSquad' hexSize pos (rotation squad) squad

hexCenter :: (Float, Int, Int) -> Position -> Point
hexCenter (size, width, height) pos = naturalOffset (size, width, height) (evenrToPixel size pos)

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

drawCell :: (Float, Int, Int) -> MixColor -> Cell -> Picture
drawCell (size, width, height) cellColor cell =
    let center = hexCenter (size, width, height) (position cell)
    in case (squad cell) of
        (Just squad) -> pictures ((polygon (hexPath (size * (1 + hexStroke)) center))
                                 :(color (mixColors (snd cellColor) 1.0 (fst cellColor) $ colorCellByTerrain cell) $ polygon (hexPath (size * (1 - hexStroke)) center))
                                 :(drawSquad size center squad)
                                 :[])
        (         _) -> pictures ((polygon (hexPath (size * (1 + hexStroke)) center))
                                 :(color (mixColors (snd cellColor) 1.0 (fst cellColor) $ colorCellByTerrain cell) $ polygon (hexPath (size * (1 - hexStroke)) center))
                                 :[])

-- draw hexogonal grid
drawCellList :: (Float, Int, Int) -> MixColor -> [Cell] -> Picture
drawCellList size color field = pictures (map (drawCell size color) field)

-- draw field, all terrain and all squads in it
drawBattle :: Battle -> Picture
drawBattle b = 
    let size = hexSize b
        sizeTuple = (size, windowWidth, windowHeight)
    in case (movingSquad b) of
    (Nothing) -> pictures ((drawCellList sizeTuple (colorSelection b $ selection b, 0.5) (possibleMoves b))
                          :(drawCellList sizeTuple (white, 0.0) (otherCells b))
                          :(drawCellList sizeTuple (white, 0.0) (allies b))
                          :(drawCellList sizeTuple (white, 0.0) (enemies b))
                          :(drawCell     sizeTuple (colorSelection b $ selection b, 2.0) $ maybe def id $ selection b)
                          :[])
    (Just ms) -> pictures ((drawCellList sizeTuple (white, 0.0) (otherCells b))
                          :(drawCellList sizeTuple (white, 0.0) (allies b))
                          :(drawCellList sizeTuple (white, 0.0) (enemies b))
                          :(drawSquad'   size (naturalOffset sizeTuple $ MovingSquad.position ms) (MovingSquad.rotation ms) (MovingSquad.squad ms))
                          :(translate (-200.0) 0.0 $ text $ show $ length $ MovingSquad.animation ms)
                          :(text $ show $ MovingSquad.rotation ms)
                          :(translate (-500.0) 200.0 $ text $ MovingSquad.getSmth $ MovingSquad.animation ms)
                          :(translate (-500.0) (-300.0) $ text $ MovingSquad.getSmth' ms $ MovingSquad.animation ms)
                          :[])

-- Game display mode.
window :: Display
window = InWindow "Game" (windowWidth, windowHeight) (10,10)

--drawGame :: Battle -> IO ()
--drawGame b = display window bgColor (drawBattle b) 