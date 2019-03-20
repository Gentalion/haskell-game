{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Battle where

import Data.Set (Set)
import qualified Data.Set as Set
import Hex (Position)
import qualified Hex as Hex

data ModifierType = ModAddWhite | ModAddGreen | ModMultiply deriving Eq
data Control = Player | EnemyAI | NoControl
data GameState = Win | Lose | Playing
type Turn = (Position, Position)
data TerrainType = TerNothing deriving Eq

data Modifier = Modifier { modName :: String
                         , modType :: ModifierType
                         , modValue :: Float
                         }

data Unit = Unit { name :: String
                 , basePower :: Float
                 , mods :: [Modifier]
                 }

data Squad = Squad { name :: String
                   , control :: Control
                   , steps :: Int
                   , maxMoveDist :: Int
                   , attackDist :: Int
                   , members :: [Unit]
                   , mods :: [Modifier]
                   }

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
                     , previousTurns :: [Turn]
                     }

-- filter all modifiers with ModifierType equal ModAddWhite and sum them
allMAW :: [Modifier] -> Float
allMAW mods = foldr (+) 0.0 (map modValue (filter (\cur -> modType cur == ModAddWhite) mods))

-- filter all modifiers with ModifierType equal ModAddGreen and sum them
allMAG :: [Modifier] -> Float
allMAG mods = foldr (+) 0.0 (map modValue (filter (\cur -> modType cur == ModAddGreen) mods))

-- filter all modifiers with ModifierType equal ModMultiply and multiply them
allMM :: [Modifier] -> Float
allMM mods = foldr (*) 1.0 (map modValue (filter (\cur -> modType cur == ModMultiply) mods))

-- calculate unit power with modifiers
unitRealPower :: Unit -> Float
unitRealPower Unit{..} = (basePower + (allMAW mods)) * (allMM mods) + (allMAG mods) -- power = (own power + white power) * power multiplier + green power

-- calculate squad power
squadPower :: Squad -> Float
squadPower = undefined

emptyCell :: Position -> Cell
emptyCell pos = Cell {position = pos, terrain = TerNothing, squad = Nothing}

-- generate field with such height and width
generateHexField :: Int -> Int -> HexField
generateHexField height width = [emptyCell (x,y) | x <- [0 .. height - 1], y <- [0 .. width - 1]]

getStraightDistance :: Cell -> Cell -> Int
getStraightDistance c1 c2 = Hex.getStraightDistance (position c1) (position c2)

-- get Cell from its position
getCellFromHexField :: HexField -> Position -> Maybe Cell
getCellFromHexField [] pos = Nothing
getCellFromHexField (x:xs) pos | (pos == position x) = Just x
                               | otherwise = getCellFromHexField xs pos

getCell :: Battle -> Position -> Maybe Cell
getCell b pos = getCellFromHexField (field b) pos

cellLeft :: Battle -> Cell -> [Cell]
cellLeft b c = maybe [] (\x -> x:[]) (getCell b (Hex.left (position c)))

cellLeftUp :: Battle -> Cell -> [Cell]
cellLeftUp b c = maybe [] (\x -> x:[]) (getCell b (Hex.leftUp (position c)))

cellLeftDown :: Battle -> Cell -> [Cell]
cellLeftDown b c = maybe [] (\x -> x:[]) (getCell b (Hex.leftDown (position c)))

cellRight :: Battle -> Cell -> [Cell]
cellRight b c = maybe [] (\x -> x:[]) (getCell b (Hex.right (position c)))

cellRightUp :: Battle -> Cell -> [Cell]
cellRightUp b c = maybe [] (\x -> x:[]) (getCell b (Hex.rightUp (position c)))

cellRightDown :: Battle -> Cell -> [Cell]
cellRightDown b c = maybe [] (\x -> x:[]) (getCell b (Hex.rightDown (position c)))

legitCells :: Battle -> [Cell] -> [Cell]
legitCells b cells =
    let h = (fieldHeight b) - 1
        w = (fieldWidth  b) - 1
    in [c | c <- cells, (fst (position c)) >=  0, (fst (position c)) <=  h, (snd (position c)) >=  0, (snd (position c)) <=  w]

getNeighbors :: Battle -> Cell -> [Cell]
getNeighbors b c = (cellLeft b c)++(cellLeftUp b c)++(cellLeftDown b c)++(cellRight b c)++(cellRightUp b c)++(cellRightDown b c)

-- second [Cell] is used as accumulating parameter
clearFromDuplicates :: [Cell] -> [Cell] -> [Cell]
clearFromDuplicates [] res = res
clearFromDuplicates (x:xs) [] = clearFromDuplicates xs (x:[])
clearFromDuplicates (x:xs) res | not (foldr (\y res -> res || position x == position y) False res) = clearFromDuplicates xs (x:res)
                               | otherwise = clearFromDuplicates xs res

-- get all other cells on distance x
getCellsOnStraightDistanceOrLess :: Int -> Battle -> Cell -> [Cell]
getCellsOnStraightDistanceOrLess 1 b c = getNeighbors b c
getCellsOnStraightDistanceOrLess n b c = clearFromDuplicates (foldr1 (++) (map (getCellsOnStraightDistanceOrLess (n-1) b) (getNeighbors b c))) []
--getCellsOnStraightDistanceOrLess n b c = Set.toList (foldr Set.union (Set.empty) (foldr (\x res -> (Set.fromList (getCellsOnStraightDistanceOrLess (n-1) b x)):res) [] (getNeighbors b c)))
-- check whether terrain is obstacle or there is a squad
isObstacle :: Cell -> Bool
isObstacle = undefined


-- get distance with obstacles
getMarchDistance :: Battle -> Position -> Position -> Int
getMarchDistance = undefined

modifyHexFieldWithCell :: HexField -> Cell -> HexField
modifyHexFieldWithCell [] _ = error "Impossible #1"
modifyHexFieldWithCell (x:xs) c | (position x) == (position c) = c:xs
                                | otherwise = modifyHexFieldWithCell xs c 

excludeCell :: [Position] -> Position -> [Position]
excludeCell [] _ = []
excludeCell (x:xs) pos | pos == x = xs
                       | otherwise = x:(excludeCell xs pos)

modifyBattleWithCell' :: Battle -> Cell -> [Position] -> [Position] -> Battle
modifyBattleWithCell' b c newAllies newEnemies = b {field = (modifyHexFieldWithCell (field b) c), allies = newAllies, enemies = newEnemies}

modifyBattleWithCell :: Battle -> Cell -> Battle
modifyBattleWithCell b c =
    let pos = position c
        prevCellOccup = case (squad (maybe (emptyCell pos) id (getCell b pos))) of
            (Nothing) -> NoControl
            (Just ps) -> control ps
        newCellOccup = case (squad c) of
            (Nothing) -> NoControl
            (Just ns) -> control ns
        bAllies = allies b
        bEnemies = enemies b
    in case (prevCellOccup, newCellOccup) of
        (NoControl, NoControl) -> modifyBattleWithCell' b c              bAllies                   bEnemies
        (NoControl,    Player) -> modifyBattleWithCell' b c         (pos:bAllies)                  bEnemies
        (NoControl,   EnemyAI) -> modifyBattleWithCell' b c              bAllies              (pos:bEnemies)
        (   Player, NoControl) -> modifyBattleWithCell' b c (excludeCell bAllies pos)              bEnemies
        (   Player,    Player) -> modifyBattleWithCell' b c              bAllies                   bEnemies
        (   Player,   EnemyAI) -> modifyBattleWithCell' b c (excludeCell bAllies pos)         (pos:bEnemies)
        (  EnemyAI, NoControl) -> modifyBattleWithCell' b c              bAllies      (excludeCell bEnemies pos)
        (  EnemyAI,    Player) -> modifyBattleWithCell' b c         (pos:bAllies)     (excludeCell bEnemies pos)
        (  EnemyAI,   EnemyAI) -> modifyBattleWithCell' b c              bAllies                   bEnemies

-- move squad from one position to another
moveSquad :: Battle -> Position -> Position -> Battle
moveSquad = undefined

-- turn for enemyAI
enemyAIturn :: Battle -> Battle
enemyAIturn = undefined

-- check whether player won, lost or is still playing
checkGameState :: Battle -> GameState
checkGameState b = case (allies b, enemies b, enemiesRemaining b) of
    ([], _,_) -> Lose
    ( _,[],0) -> Win
    ( _, _,_) -> Playing
