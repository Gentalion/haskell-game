module InteractBattle where

import Graphics.Gloss.Interface.Pure.Game
import Battle
import Squad
import Const
import Hex (Position)
import Data.Default
import System.Exit
import Distances

handleInput :: Event -> Battle -> IO Battle
handleInput event b = 
    let size = (hexMaximumInWindowSize windowWidth windowHeight (fieldWidth b) (fieldHeight b), windowWidth, windowHeight)
        hasControl = control $ maybe def id $ squad $ maybe def id $ selection b
    in case (event, hasControl, animation b) of
        (EventKey ( SpecialKey     KeyEsc) Down _ pos,         _,[]) -> exitSuccess
        (EventKey ( SpecialKey   KeySpace)   Up _ pos,         _,[]) -> return $ enemyTurn b
        (EventKey (MouseButton LeftButton) Down _ pos, NoControl,[]) -> return $ selectPosition b $ pixelToEvenr size pos
        (EventKey (MouseButton LeftButton) Down _ pos,    Player,[]) -> return $ secondClickAfterSelection b $ pixelToEvenr size pos
        (EventKey (MouseButton LeftButton) Down _ pos,     Enemy,[]) -> return $ secondClickAfterSelection b $ pixelToEvenr size pos
        (                                           _,         _, _) -> return b

enemyTurn :: Battle -> Battle
enemyTurn b =
    let b1 = removeSelection b
    in b1 {movingEnemies = enemies b1}
    --in enemyAIturn b1 (enemies b1)

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
        (Player) -> b {selection = Just cell, possibleMoves = possibleMoves_, otherCells = excludeCells (otherCells b) possibleMoves_, allies = excludeCell (allies b) cell}
        ( Enemy) -> b {selection = Just cell, possibleMoves = possibleMoves_, otherCells = excludeCells (otherCells b) possibleMoves_, enemies = excludeCell (enemies b) cell}

secondClickAfterSelection :: Battle -> Position -> Battle
secondClickAfterSelection b pos = 
    let cell = getCell b pos
        selected = maybe def id $ selection b
    in case (position cell == position selected, control $ maybe def id $ squad selected, member cell (possibleMoves b)) of
        (True,     _,    _) -> removeSelection b
        (   _,     _,False) -> selectPosition (removeSelection b) pos
        (   _,Player, True) -> moveSquadAnimated b (hexSize b) selected cell
        (   _, Enemy,    _) -> b