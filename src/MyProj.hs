{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MyProj
    ( runTheGame, getStraightDistance
    ) where

import Graphics.Gloss.Data.Picture

data ModifierType = ModAddWhite | ModAddGreen | ModMultiply deriving Eq
data Control = Player | EnemyAI
data GameState = Win | Lose | Playing
type Position = (Int, Int) -- first Int stands for column, second for row
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
                   , members :: [Unit]
                   , mods :: [Modifier]
                   }

data Cell = Cell { terrain :: TerrainType
                 , squad :: (Maybe Squad)
                 }

type HexField = [(Position, Cell)]

data Battle = Battle { field :: HexField
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
generateHexField height width = [((x, y), Cell {terrain = TerNothing, squad = Nothing}) | x <- [0..height-1], y <- [0..width-1]]

-- check whether terrain is obstacle or there is a squads
isObstacle :: Cell -> Bool
isObstacle = undefined

-- get all other cells on distance x
getCellsOnStraightDistanceOrLess :: Int -> Battle -> Position -> [Position]
getCellsOnStraightDistanceOrLess = undefined

-- it's magic or math
hexOffsetX :: Int -> Int -> Int
hexOffsetX dx dy =
    let xSign = signum dx
        ySign = signum dy 
        yMod2 = mod dy 2
    in case (xSign, ySign, yMod2) of
        (-1,-1, 0) ->  0
        (-1,-1, 1) -> -1
        ( 1, 1, 0) ->  1
        ( 1, 1, 1) ->  0
        (-1, 1, 0) -> -1
        (-1, 1, 1) ->  0
        ( 1,-1, 0) ->  0
        ( 1,-1, 1) ->  1
        otherwise  ->  0

-- it's definitely not magic
hexOffsetY :: Int -> Int -> Int
hexOffsetY dx dy = signum dy

-- get distance without obstacles
getStraightDistance :: Position -> Position -> Int
getStraightDistance (x1,y1) (x2,y2) = 
    let dx = x1 - x2
        dy = y1 - y2
    in case (abs dx, abs dy) of
        (0, n) -> n
        (n, 0) -> n
        (a, b) -> 1 + getStraightDistance (x1, y1) (x2 + hexOffsetX dx dy, y2 + hexOffsetY dx dy)

-- get distance with obstacles
getMarchDistance :: Battle -> Position -> Position -> Int
getMarchDistance = undefined

-- move squad from one position to another
moveSquad :: Battle -> Position -> Position -> Battle
moveSquad = undefined

-- turn for enemyAI
enemyAIturn :: Battle -> Battle
enemyAIturn = undefined

-- draw interface outside of battle
drawMenu :: Picture
drawMenu = undefined

-- draw single unit
drawUnit :: Unit -> Picture
drawUnit = undefined

-- draw single squad i.e. bunch of units
drawSquad :: Squad -> Picture
drawSquad = undefined

-- draw field, all terrain and all squads in it
drawBattleScene :: Battle -> Picture
drawBattleScene = undefined

-- check whether player won, lost or is still playing
checkGameState :: Battle -> GameState
checkGameState = undefined

runTheGame :: IO ()
runTheGame = putStrLn "This is a proud little game!"
