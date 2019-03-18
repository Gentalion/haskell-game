{-# LANGUAGE RecordWildCards #-}

module MyProj
    ( runMyProj
    ) where

import Graphics.Gloss.Data.Picture

data ModifierType = ModAddWhite | ModAddGreen | ModMultiply deriving Eq
data Control = Player | EnemyAI
data GameState = Win | Lose | Playing
type Position = (Int, Int)
data TerrainType = TerNothing deriving Eq

data Modifier = Modifier { modName :: String
                         , modType :: ModifierType
                         , modValue :: Float
                         }

data Unit = Unit { unitName :: String
                 , unitControl :: Control
                 , unitBasePower :: Float
                 , unitMods :: [Modifier]
                 }

data Squad = Squad { squadName :: String
                   , squadMembers :: [Unit]
                   , squadMods :: [Modifier]
                   }

data Cell = Cell { sqTerrain :: TerrainType
                 , sqSquad :: (Maybe Squad)
                 , neighbors :: [Position]
                 }

type Field = [(Position, Cell)]

allMAW :: [Modifier] -> Float -- filter all modifiers with ModifierType equal ModAddWhite and add them
allMAW mods = foldr (+) 0.0 (map modValue (filter (\cur -> modType cur == ModAddWhite) mods))

allMAG :: [Modifier] -> Float -- filter all modifiers with ModifierType equal ModAddGreen and add them
allMAG mods = foldr (+) 0.0 (map modValue (filter (\cur -> modType cur == ModAddGreen) mods))

allMM :: [Modifier] -> Float -- filter all modifiers with ModifierType equal ModMultiply and multiply them
allMM mods = foldr (*) 1.0 (map modValue (filter (\cur -> modType cur == ModMultiply) mods))

unitRealPower :: Unit -> Float -- calculate unit power with modifiers
unitRealPower Unit{..} = (unitBasePower + (allMAW unitMods)) * (allMM unitMods) + (allMAG unitMods)

squadPower :: Squad -> Float -- calculate squad power
squadPower = undefined

generateField :: Int -> Field -- generate Field with n Cell's
generateField = undefined

getCellsOnDistance :: Int -> Position -> Field -> [Cell] -- get all other Cell's on distance x
getCellsOnDistance = undefined

moveSquad :: Field -> Position -> Position -> Field -- move squad from one position to another
moveSquad = undefined

enemyAIturn :: Field -> Field -- turn for enemyAI
enemyAIturn = undefined

drawMenu :: Picture -- draw interface outside of battle
drawMenu = undefined

drawUnit :: Unit -> Picture -- draw single unit
drawUnit = undefined

drawSquad :: Squad -> Picture -- draw single squad i.e. bunch of units
drawSquad = undefined

drawBattleScene :: Field -> Picture -- draw Field, all terrain and all Squads in it
drawBattleScene = undefined

checkGameState :: Field -> GameState -- check whether a player won, lost or is still playing
checkGameState = undefined

runMyProj :: IO ()
runMyProj = putStrLn "This is a demo project!"
