{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MyProj
    ( runTheGame
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

type Field = [(Position, Cell)]

data Battle = Battle { field :: Field
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

-- generate field as hexogonal grid with such height and width
generateHexGrid :: Int -> Int -> Field
generateHexGrid height width = [((x, y), Cell {terrain = TerNothing, squad = Nothing}) | x <- [0..height-1], y <- [0..width-1]]

-- check whether terrain is obstacle or there is a squads
isObstacle :: Cell -> Bool
isObstacle = undefined

-- get all other cells on distance x
getCellsOnStraightDistanceOrLess :: Int -> Battle -> Position -> [Position]
getCellsOnStraightDistanceOrLess = undefined

-- get distance without obstacles
getStraightDistance :: Battle -> Position -> Position -> Int
getStraightDistance = undefined

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
