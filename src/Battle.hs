{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Battle where

import Data.Set (Set)
import qualified Data.Set as Set
import Hex (Position)
import qualified Hex as Hex
import Squad
import Data.Default

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
                     }

instance Default Battle where
    def = Battle {otherCells = [], fieldHeight = 0, fieldWidth = 0, allies = [], enemies = [], enemiesRemaining = 0, selection = Nothing, possibleMoves = [], previousTurns = []}

-- generate field with such height and width
generateHexField :: Int -> Int -> [Cell]
generateHexField width height = [def {position = (x,y), terrain = TerPlain} | x <- [0 .. width - 1], y <- [0 .. height - 1]]

getStraightDistance :: Cell -> Cell -> Int
getStraightDistance c1 c2 = Hex.getStraightDistance (position c1) (position c2)

-- get Cell from its position
getCellFromCellCollection :: [Cell] -> Position -> Cell
getCellFromCellCollection [] pos = ((def :: Cell) {position = pos})
getCellFromCellCollection (x:xs) pos | (pos == position x) = x
                               | otherwise = getCellFromCellCollection xs pos

field :: Battle -> [Cell]
field b = case (selection b) of
    (Nothing) -> (otherCells b)++(allies b)++(enemies b)
    ( Just c) -> c:(otherCells b)++(allies b)++(enemies b)++(possibleMoves b)


getCell :: Battle -> Position -> Cell
getCell b pos = getCellFromCellCollection (field b) pos

cellLeft :: Battle -> Cell -> Cell
cellLeft b c = getCell b $ Hex.left (position c)

cellLeftUp :: Battle -> Cell -> Cell
cellLeftUp b c = getCell b $ Hex.leftUp (position c)

cellLeftDown :: Battle -> Cell -> Cell
cellLeftDown b c = getCell b $ Hex.leftDown (position c)

cellRight :: Battle -> Cell -> Cell
cellRight b c = getCell b $ Hex.right (position c)

cellRightUp :: Battle -> Cell -> Cell
cellRightUp b c = getCell b $ Hex.rightUp (position c)

cellRightDown :: Battle -> Cell -> Cell
cellRightDown b c = getCell b $ Hex.rightDown (position c)

legitCells :: Battle -> [Cell] -> [Cell]
legitCells b cells =
    let h = (fieldHeight b)
        w = (fieldWidth  b)
    in [c | c <- cells, (fst $ position c) >=  0, (fst $ position c) <  w, (snd $ position c) >=  0, (snd $ position c) <  h]

getNeighbors :: Battle -> Cell -> [Cell]
getNeighbors b c = legitCells b $ (cellLeft b c):(cellLeftUp b c):(cellLeftDown b c):(cellRight b c):(cellRightUp b c):(cellRightDown b c):[]

-- second [Cell] is used as accumulating parameter
clearFromDuplicates :: [Cell] -> [Cell] -> [Cell]
clearFromDuplicates [] res = res
clearFromDuplicates (x:xs) [] = clearFromDuplicates xs (x:[])
clearFromDuplicates (x:xs) res | not (foldr (\y res -> res || position x == position y) False res) = clearFromDuplicates xs (x:res)
                               | otherwise = clearFromDuplicates xs res

excludeCell :: [Cell] -> Cell -> [Cell]
excludeCell [] _ = []
excludeCell (x:xs) c | (position x) == (position c) = xs
                     | otherwise = x:(excludeCell xs c)

excludeCells :: [Cell] -> [Cell] -> [Cell]
excludeCells [] _ = []
excludeCells x [] = x
excludeCells x (y:ys) = excludeCells (excludeCell x y) ys

-- get all other cells on distance x
getCellsOnStraightDistanceOrLess' :: Int -> Battle -> Cell -> [Cell]
getCellsOnStraightDistanceOrLess' 1 b c = getNeighbors b c
getCellsOnStraightDistanceOrLess' n b c = foldr1 (++) $ map (getCellsOnStraightDistanceOrLess (n-1) b) (getNeighbors b c)

getCellsOnStraightDistanceOrLess :: Int -> Battle -> Cell -> [Cell]
getCellsOnStraightDistanceOrLess n b c = excludeCell (clearFromDuplicates (getCellsOnStraightDistanceOrLess' n b c) []) c

-- check whether terrain is obstacle or there is a squad
notObstacle :: Cell -> Bool
notObstacle c = case (terrain c, squad c) of
    (_       , Just _) -> False
    (TerWater,      _) -> False
    (       _,      _) -> True

getCellsOnMarchDistanceOrLess' :: Int -> Battle -> Cell -> [Cell]
getCellsOnMarchDistanceOrLess' 1 b c = filter notObstacle (getCellsOnStraightDistanceOrLess 1 b c)
getCellsOnMarchDistanceOrLess' n b c = foldr (\x res -> (getCellsOnMarchDistanceOrLess (n-1) b x)++res) [] (filter notObstacle $ getCellsOnStraightDistanceOrLess 1 b c)

getCellsOnMarchDistanceOrLess :: Int -> Battle -> Cell -> [Cell]
getCellsOnMarchDistanceOrLess n b c = clearFromDuplicates (getCellsOnMarchDistanceOrLess' n b c) []

getMarchDistance' :: Int -> Battle -> Cell -> Cell -> Int
getMarchDistance' n b c1 c2 | (n > (fieldHeight b)) && (n > (fieldWidth b)) = error "Impossible #2"
                            | any (\x -> position x == position c2) (getCellsOnMarchDistanceOrLess n b c1) = n
                            | otherwise = getMarchDistance' (n+1) b c1 c2

getPossibleMoves :: Battle -> Cell -> Int -> [Cell]
getPossibleMoves b@Battle{..} c n =
    let newField = otherCells++possibleMoves
    in getCellsOnMarchDistanceOrLess n b c

-- get distance with obstacles
getMarchDistance :: Battle -> Cell -> Cell -> Int
getMarchDistance b c1 c2 = getMarchDistance' 1 b c1 c2

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

        (NoControl,   EnemyAI) -> b {otherCells = excludeCell (otherCells b) c
                                    ,enemies = c:(enemies b)}

        (   Player, NoControl) -> b {otherCells = c:(otherCells b)
                                    ,allies = excludeCell (allies b) c}

        (   Player,    Player) -> b {allies = modifyCellCollection (allies b) c}

        (   Player,   EnemyAI) -> b {allies = excludeCell (allies b) c
                                    ,enemies = c:(enemies b)}

        (  EnemyAI, NoControl) -> b {otherCells = c:(otherCells b)
                                    ,enemies = excludeCell (enemies b) c}

        (  EnemyAI,    Player) -> b {allies = c:(allies b)
                                    ,enemies = excludeCell (enemies b) c}

        (  EnemyAI,   EnemyAI) -> b {enemies = modifyCellCollection (enemies b) c}

removeSelection :: Battle -> Battle
removeSelection b =
    let selected = maybe def id $ selection b
        b' = removeSelection' b
    in case (control $ maybe def id $ squad selected) of
        (NoControl) -> b' {otherCells = selected:(otherCells b')}
        (   Player) -> b' {allies = selected:(allies b')}
        (  EnemyAI) -> b' {enemies = selected:(enemies b')}

removeSelection' :: Battle -> Battle
removeSelection' b = b {otherCells = (otherCells b)++(possibleMoves b), selection = Nothing, possibleMoves = []}

moveSquad :: Battle -> Cell -> Cell -> Battle
moveSquad b c1 c2 = 
    let c1Squad = squad c1
        c2Squad = squad c2
    in case (c1Squad, c2Squad) of
        (Nothing, Nothing) -> error "Impossible #3"
        ( Just x, Nothing) -> modifyBattleWithCell (c1 {squad = Nothing}) $ modifyBattleWithCell (c2 {squad = Just x}) $ removeSelection' b
        (Nothing,  Just y) -> modifyBattleWithCell (c1 {squad = Just y}) $ modifyBattleWithCell (c2 {squad = Nothing}) $ removeSelection' b
        ( Just x,  Just y) -> error "Impossible #4"

-- turn for enemyAI
enemyAIturn :: Battle -> Battle
enemyAIturn = undefined

-- check whether player won, lost or is still playing
checkGameState :: Battle -> GameState
checkGameState b = case (allies b, enemies b, enemiesRemaining b) of
    ([], _,_) -> Lose
    ( _,[],0) -> Win
    ( _, _,_) -> Playing

member :: Cell -> [Cell] -> Bool
member cell cellList = foldr (\y res -> res || (position cell) == (position y)) False cellList