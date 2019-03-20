{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Battle where

data ModifierType = ModAddWhite | ModAddGreen | ModMultiply deriving Eq
data Control = Player | EnemyAI
data GameState = Win | Lose | Playing
type Position = (Int, Int) -- first int stands for column, second for row
type Turn = (Position, Position)
data TerrainType = TerNothing deriving Eq

data Modifier = Modifier { modName :: String
                         , modType :: ModifierType
                         , modValue :: Float
                         }

data Unit = Unit { name :: String
                 , control :: Control
                 , basePower :: Float
                 , mods :: [Modifier]
                 }

data Squad = Squad { name :: String
                   , steps :: Int
                   , maxMoveDist :: Int
                   , attackDist :: Int
                   , members :: [Unit]
                   , mods :: [Modifier]
                   }

data Cell = Cell { position :: Position
                 , terrain :: TerrainType
                 , squad :: (Maybe Squad)
                 }

type HexField = [Cell] -- we consider our field to be "even-r" hexagonal grid like it's shown here https://www.redblobgames.com/grids/hexagons/

data Battle = Battle { field :: HexField
                     , fieldHeight :: Int
                     , fieldWidth :: Int
                     , allies :: [Position]
                     , enemies :: [Position]
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

-- generate field with such height and width
generateHexField :: Int -> Int -> HexField
generateHexField height width = [Cell {position = (x, y), terrain = TerNothing, squad = Nothing} | x <- [0 .. height - 1], y <- [0 .. width - 1]]

hexLeft :: Position -> Position
hexLeft (x, y) = (x - 1, y)

hexLeftUp :: Position -> Position
hexLeftUp (x, y) | (mod y 2) == 1 = (x - 1, y - 1)
                 | otherwise      = (x    , y - 1)

hexLeftDown :: Position -> Position
hexLeftDown (x, y) | (mod y 2) == 1 = (x - 1, y + 1)
                   | otherwise      = (x    , y + 1)

hexRight :: Position -> Position
hexRight (x, y) = (x + 1, y)

hexRightUp :: Position -> Position
hexRightUp (x, y) | (mod y 2) == 1 = (x    , y - 1)
                  | otherwise      = (x + 1, y - 1)

hexRightDown :: Position -> Position
hexRightDown (x, y) | (mod y 2) == 1 = (x    , y + 1)
                    | otherwise      = (x + 1, y + 1)

-- get distance without obstacles
getStraightDistanceByPos :: Position -> Position -> Int
getStraightDistanceByPos (x1,y1) (x2,y2) =
    let dx = x1 - x2
        dy = y1 - y2
    in case (abs dx, abs dy, signum dx, signum dy) of
        (0,n, _, _) -> n
        (n,0, _, _) -> n
        (_,_, 1, 1) -> 1 + getStraightDistanceByPos (hexLeftUp    (x1,y1)) (x2,y2)
        (_,_,-1,-1) -> 1 + getStraightDistanceByPos (hexRightDown (x1,y1)) (x2,y2)
        (_,_,-1, 1) -> 1 + getStraightDistanceByPos (hexRightUp   (x1,y1)) (x2,y2)
        (_,_, 1,-1) -> 1 + getStraightDistanceByPos (hexLeftDown  (x1,y1)) (x2,y2)

getStraightDistance :: Cell -> Cell -> Int
getStraightDistance c1 c2 = getStraightDistanceByPos (position c1) (position c2)

-- get Cell from its position
getCellFromHexField :: HexField -> Position -> Maybe Cell
getCellFromHexField [] pos = Nothing
getCellFromHexField (x:xs) pos | (pos == position x) = Just x
                               | otherwise = getCellFromHexField xs pos

getCell :: Battle -> Position -> Maybe Cell
getCell b pos = getCellFromHexField (field b) pos

cellLeft :: Battle -> Cell -> [Cell]
cellLeft b c = maybe [] (\x -> x:[]) (getCell b (hexLeft (position c)))

cellLeftUp :: Battle -> Cell -> [Cell]
cellLeftUp b c = maybe [] (\x -> x:[]) (getCell b (hexLeftUp (position c)))

cellLeftDown :: Battle -> Cell -> [Cell]
cellLeftDown b c = maybe [] (\x -> x:[]) (getCell b (hexLeftDown (position c)))

cellRight :: Battle -> Cell -> [Cell]
cellRight b c = maybe [] (\x -> x:[]) (getCell b (hexRight (position c)))

cellRightUp :: Battle -> Cell -> [Cell]
cellRightUp b c = maybe [] (\x -> x:[]) (getCell b (hexRightUp (position c)))

cellRightDown :: Battle -> Cell -> [Cell]
cellRightDown b c = maybe [] (\x -> x:[]) (getCell b (hexRightDown (position c)))

legitCells :: Battle -> [Cell] -> [Cell]
legitCells b cells =
    let h = (fieldHeight b) - 1
        w = (fieldWidth  b) - 1
    in [c | c <- cells, (fst (position c)) >=  0, (fst (position c)) <=  h, (snd (position c)) >=  0, (snd (position c)) <=  w]

getNeighbors :: Battle -> Cell -> [Cell]
getNeighbors b c = (cellLeft b c)++(cellLeftUp b c)++(cellLeftDown b c)++(cellRight b c)++(cellRightUp b c)++(cellRightDown b c)

-- get all other cells on distance x
getCellsOnStraightDistanceOrLess :: Int -> Battle -> Position -> [Position]
getCellsOnStraightDistanceOrLess = undefined

-- check whether terrain is obstacle or there is a squad
isObstacle :: Cell -> Bool
isObstacle = undefined


-- get distance with obstacles
getMarchDistance :: Battle -> Position -> Position -> Int
getMarchDistance = undefined

-- move squad from one position to another
moveSquad :: Battle -> Position -> Position -> Battle
moveSquad = undefined

-- turn for enemyAI
enemyAIturn :: Battle -> Battle
enemyAIturn = undefined

-- check whether player won, lost or is still playing
checkGameState :: Battle -> GameState
checkGameState = undefined
