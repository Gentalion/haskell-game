{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Battle where

import Hex (Position)
import Squad (Squad(..), Control(..))
import Data.Default
import MovingSquad (MovingSquad)
import Graphics.Gloss (Point)
import Const

data GameState = Win | Lose | Playing
data TerrainType = TerNothing | TerPlain | TerWater deriving Eq

instance Default TerrainType where
    def = TerNothing

data Cell = Cell { position :: Position
                 , terrain :: TerrainType
                 , squad :: Maybe Squad
                 }

instance Default Cell where
    def = Cell {position = (-2,-2), terrain = def, squad = Nothing} 

--type HexField = [Cell] -- we consider our field to be "even-r" hexagonal grid like it's shown here https://www.redblobgames.com/grids/hexagons/

type SquadPos = (Position, Float)
type Step = (SquadPos, SquadPos)
type Turn = [Step]

data Battle = Battle { otherCells :: [Cell]
                     , fieldHeight :: Int
                     , fieldWidth :: Int
                     , allies :: [Cell]
                     , enemies :: [Cell]
                     , enemiesRemaining :: Int
                     , selection :: Maybe Cell
                     , possibleMoves :: [Cell]
                     , previousTurns :: [Turn]
                     , movingSquad :: Maybe MovingSquad
                     }

instance Default Battle where
    def = Battle { otherCells = []
                 , fieldHeight = 0
                 , fieldWidth = 0
                 , allies = []
                 , enemies = []
                 , enemiesRemaining = 0
                 , selection = Nothing
                 , possibleMoves = []
                 , previousTurns = []
                 , movingSquad = Nothing
                 }

-- generate field with such height and width
generateHexField :: Int -> Int -> [Cell]
generateHexField width height = [def {position = (x,y), terrain = TerPlain} | x <- [0 .. width - 1], y <- [0 .. height - 1]]

-- get Cell from its position
getCellFromCellCollection :: [Cell] -> Position -> Cell
getCellFromCellCollection [] pos = ((def :: Cell) {position = pos})
getCellFromCellCollection (x:xs) pos | (pos == position x) = x
                               | otherwise = getCellFromCellCollection xs pos

getCell :: Battle -> Position -> Cell
getCell b pos = getCellFromCellCollection (field b) pos

field :: Battle -> [Cell]
field b = case (selection b) of
    (Nothing) -> (otherCells b)++(allies b)++(enemies b)
    ( Just c) -> c:(otherCells b)++(allies b)++(enemies b)++(possibleMoves b)

hexSize :: Battle -> Float
hexSize b = hexMaximumInWindowSize windowWidth windowHeight (fieldWidth b) (fieldHeight b)

excludeCell :: [Cell] -> Cell -> [Cell]
excludeCell [] _ = []
excludeCell (x:xs) c | (position x) == (position c) = xs
                     | otherwise = x:(excludeCell xs c)

excludeCells :: [Cell] -> [Cell] -> [Cell]
excludeCells [] _ = []
excludeCells x [] = x
excludeCells x (y:ys) = excludeCells (excludeCell x y) ys

modifyCellCollection :: [Cell] -> Cell -> [Cell]
modifyCellCollection [] c = [c]
modifyCellCollection (x:xs) c | (position x) == (position c) = c:xs
                              | otherwise = x:(modifyCellCollection xs c)

-- works correctly for Battle with no selection and empty possibleMoves i.e. for internal calls
modifyBattleWithCell :: Cell -> Battle -> Battle
modifyBattleWithCell c b =
    let pos = position c
        prevCellOccup = case (squad (getCell b pos)) of
            (Nothing) -> NoControl
            (Just ps) -> control ps
        newCellOccup = case (squad c) of
            (Nothing) -> NoControl
            (Just ns) -> control ns
    in case (prevCellOccup, newCellOccup) of
        (NoControl, NoControl) -> b {otherCells = modifyCellCollection (otherCells b) c}

        (NoControl,    Player) -> b {otherCells = excludeCell (otherCells b) c
                                    ,allies = c:(allies b)}

        (NoControl,   Enemy) -> b {otherCells = excludeCell (otherCells b) c
                                    ,enemies = c:(enemies b)}

        (   Player, NoControl) -> b {otherCells = c:(otherCells b)
                                    ,allies = excludeCell (allies b) c}

        (   Player,    Player) -> b {allies = modifyCellCollection (allies b) c}

        (   Player,   Enemy) -> b {allies = excludeCell (allies b) c
                                    ,enemies = c:(enemies b)}

        (  Enemy, NoControl) -> b {otherCells = c:(otherCells b)
                                    ,enemies = excludeCell (enemies b) c}

        (  Enemy,    Player) -> b {allies = c:(allies b)
                                    ,enemies = excludeCell (enemies b) c}

        (  Enemy,   Enemy) -> b {enemies = modifyCellCollection (enemies b) c}