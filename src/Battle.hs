{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Battle where

import Data.Set (Set)
import qualified Data.Set as Set
import Hex (Position)
import qualified Hex as Hex
import Squad

data GameState = Win | Lose | Playing
type Turn = (Position, Position)
data TerrainType = TerNothing | TerPlain | TerWater

data Cell = Cell { position :: Position
                 , terrain :: TerrainType
                 , squad :: Maybe Squad
                 }

type HexField = [Cell] -- we consider our field to be "even-r" hexagonal grid like it's shown here https://www.redblobgames.com/grids/hexagons/

data Battle = Battle { field :: HexField
                     , fieldHeight :: Int
                     , fieldWidth :: Int
                     , allies :: [Position]
                     , enemies :: [Position]
                     , enemiesRemaining :: Int
                     , selection :: Maybe Position
                     , possibleMoves :: [Position]
                     , previousTurns :: [Turn]
                     }

emptyCell :: Position -> Cell
emptyCell pos = Cell {position = pos, terrain = TerNothing, squad = Nothing}

-- generate field with such height and width
generateHexField :: Int -> Int -> HexField
generateHexField height width = [(emptyCell (x,y)) {terrain = TerPlain} | x <- [0 .. width - 1], y <- [0 .. height - 1]]

getStraightDistance :: Cell -> Cell -> Int
getStraightDistance c1 c2 = Hex.getStraightDistance (position c1) (position c2)

-- get Cell from its position
getCellFromHexField :: HexField -> Position -> Cell
getCellFromHexField [] pos = emptyCell pos
getCellFromHexField (x:xs) pos | (pos == position x) = x
                               | otherwise = getCellFromHexField xs pos

getCell :: Battle -> Position -> Cell
getCell b pos = getCellFromHexField (field b) pos

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
    let h = (fieldHeight b) - 1
        w = (fieldWidth  b) - 1
    in [c | c <- cells, (fst $ position c) >=  0, (fst $ position c) <=  h, (snd $ position c) >=  0, (snd $ position c) <=  w]

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


-- get distance with obstacles
getMarchDistance :: Battle -> Cell -> Cell -> Int
getMarchDistance b c1 c2 = getMarchDistance' 1 b c1 c2

modifyHexFieldWithCell :: HexField -> Cell -> HexField
modifyHexFieldWithCell [] _ = error "Impossible #1"
modifyHexFieldWithCell (x:xs) c | (position x) == (position c) = c:xs
                                | otherwise = x:(modifyHexFieldWithCell xs c)

excludePosition :: [Position] -> Position -> [Position]
excludePosition [] _ = []
excludePosition (x:xs) pos | pos == x = xs
                       | otherwise = x:(excludePosition xs pos)

modifyBattleWithCell' :: Battle -> Cell -> [Position] -> [Position] -> Battle
modifyBattleWithCell' b c newAllies newEnemies = b {field = (modifyHexFieldWithCell (field b) c), allies = newAllies, enemies = newEnemies}

modifyBattleWithCell :: Battle -> Cell -> Battle
modifyBattleWithCell b c =
    let pos = position c
        prevCellOccup = case (squad (getCell b pos)) of
            (Nothing) -> NoControl
            (Just ps) -> control ps
        newCellOccup = case (squad c) of
            (Nothing) -> NoControl
            (Just ns) -> control ns
        bAllies = allies b
        bEnemies = enemies b
    in case (prevCellOccup, newCellOccup) of
        (NoControl, NoControl) -> modifyBattleWithCell' b c                  bAllies                       bEnemies
        (NoControl,    Player) -> modifyBattleWithCell' b c             (pos:bAllies)                      bEnemies
        (NoControl,   EnemyAI) -> modifyBattleWithCell' b c                  bAllies                  (pos:bEnemies)
        (   Player, NoControl) -> modifyBattleWithCell' b c (excludePosition bAllies pos)                  bEnemies
        (   Player,    Player) -> modifyBattleWithCell' b c                  bAllies                       bEnemies
        (   Player,   EnemyAI) -> modifyBattleWithCell' b c (excludePosition bAllies pos)             (pos:bEnemies)
        (  EnemyAI, NoControl) -> modifyBattleWithCell' b c                  bAllies      (excludePosition bEnemies pos)
        (  EnemyAI,    Player) -> modifyBattleWithCell' b c             (pos:bAllies)     (excludePosition bEnemies pos)
        (  EnemyAI,   EnemyAI) -> modifyBattleWithCell' b c                  bAllies                       bEnemies

-- move squad from one position to another
moveSquad :: Battle -> Position -> Position -> Battle
moveSquad b p1 p2 = 
    let p1Cell = getCell b p1
        p2Cell = getCell b p2
        p1Squad = squad p1Cell
        p2Squad = squad p2Cell
        p1ResCell = p1Cell {squad = p2Squad}
        p2ResCell = p2Cell {squad = p1Squad}
    in modifyBattleWithCell (modifyBattleWithCell b p1ResCell) p2ResCell

-- turn for enemyAI
enemyAIturn :: Battle -> Battle
enemyAIturn = undefined

-- check whether player won, lost or is still playing
checkGameState :: Battle -> GameState
checkGameState b = case (allies b, enemies b, enemiesRemaining b) of
    ([], _,_) -> Lose
    ( _,[],0) -> Win
    ( _, _,_) -> Playing
