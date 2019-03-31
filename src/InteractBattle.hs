module InteractBattle where

import Graphics.Gloss.Interface.Pure.Game
import Battle
import Squad
import Const
import Hex (Position)
import Data.Default

pixelToEvenr :: (Float, Int, Int) -> Point -> Position
pixelToEvenr (size, width, height) (x1,y1) =
    let (x2,y2) = (naturalOffset (size, width, height) (0.0,0.0))
        (x,y) = (x1-x2,y1-y2)
        row = round (- 2/3 * y / size)
        col = round ((x + 0.5 * (fromIntegral (mod row 2)) * size * sqrt 3.0) / size / sqrt 3.0)
    in (col, row)

handleInput :: Event -> Battle -> Battle
handleInput event b = 
    let size = (hexMaximumInWindowSize windowWidth windowHeight (fieldWidth b) (fieldHeight b), windowWidth, windowHeight)
        hasControl = control $ maybe def id $ squad $ maybe def id $ selection b
    in case (event, hasControl, movingSquad b) of
        (                                           _,         _, Just _) -> b
        (EventKey (MouseButton LeftButton) Down _ pos, NoControl,      _) -> selectPosition b $ pixelToEvenr size pos
        (EventKey (MouseButton LeftButton) Down _ pos,    Player,      _) -> secondClickAfterSelection b $ pixelToEvenr size pos
        (EventKey (MouseButton LeftButton) Down _ pos,   EnemyAI,      _) -> secondClickAfterSelection b $ pixelToEvenr size pos
        (                                           _,         _,      _) -> b

selectPosition :: Battle -> Position -> Battle
selectPosition b (x,y) =
    let cell = getCell b (x,y)
        isLegitCell = x >= 0 && y >= 0 && x < fieldWidth b && y < fieldHeight b
    in case (isLegitCell, squad cell) of
        (False,  _) -> b
        (_,Nothing) -> b {selection = Just cell}
        (_,Just  _) -> selectCellWithSquad b cell

selectCellWithSquad :: Battle -> Cell -> Battle
selectCellWithSquad b cell = 
    let squad_ = maybe def id $ squad cell
        control_ = control squad_
        possibleMoves_ = getPossibleMoves b cell $ steps squad_
    in case (control_) of
        ( Player) -> b {selection = Just cell, possibleMoves = possibleMoves_, otherCells = excludeCells (otherCells b) possibleMoves_, allies = excludeCell (allies b) cell}
        (EnemyAI) -> b {selection = Just cell, possibleMoves = possibleMoves_, otherCells = excludeCells (otherCells b) possibleMoves_, enemies = excludeCell (enemies b) cell}

secondClickAfterSelection :: Battle -> Position -> Battle
secondClickAfterSelection b pos = 
    let cell = getCell b pos
        selected = maybe def id $ selection b
    in case (position cell == position selected, control $ maybe def id $ squad selected, member cell (possibleMoves b)) of
        (True,      _,    _) -> removeSelection b
        (   _,      _,False) -> selectPosition (removeSelection b) pos
        (   _, Player, True) -> moveSquad b cell selected
        (   _,EnemyAI,    _) -> b