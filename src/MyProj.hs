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
                         , modValue :: Double
                         }

data Unit = Unit { unitName :: String
                 , unitControl :: Control
                 , unitBasePower :: Double
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

allMAW :: [Modifier] -> Double -- filter all modifiers with ModifierType equal ModAddWhite and add them
allMAW mods = foldr (+) 0.0 (map modValue (filter (\cur -> modType cur == ModAddWhite) mods))

allMAG :: [Modifier] -> Double -- filter all modifiers with ModifierType equal ModAddGreen and add them
allMAG mods = foldr (+) 0.0 (map modValue (filter (\cur -> modType cur == ModAddGreen) mods))

allMM :: [Modifier] -> Double -- filter all modifiers with ModifierType equal ModMultiply and multiply them
allMM mods = foldr (*) 1.0 (map modValue (filter (\cur -> modType cur == ModMultiply) mods))

unitRealPower :: Unit -> Double
unitRealPower Unit{..} = (unitBasePower + (allMAW unitMods)) * (allMM unitMods) + (allMAG unitMods)

squadPower :: Squad -> Double
squadPower = undefined

generateField :: Int -> Field
generateField = undefined

getCellsOnDistance :: Int -> Position -> Field -> [Cell]
getCellsOnDistance = undefined

moveSquad :: Field -> Position -> Position -> Field
moveSquad = undefined

enemyAIturn :: Field -> Field
enemyAIturn = undefined

drawMenu :: Picture
drawMenu = undefined

drawUnit :: Unit -> Picture
drawUnit = undefined

drawSquad :: Squad -> Picture
drawSquad = undefined

drawBattleScene :: Field -> Picture
drawBattleScene = undefined

checkGameState :: Field -> GameState
checkGameState = undefined

runMyProj :: IO ()
runMyProj = putStrLn "This is a demo project!"
