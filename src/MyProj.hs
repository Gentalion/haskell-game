{-# LANGUAGE RecordWildCards #-}

module MyProj
    ( runMyProj
    ) where

data ModifierType = ModAddWhite | ModAddGreen | ModMultiply deriving Eq
data Control = Player | EnemyAI
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

data Square = Square { sqTerrain :: TerrainType
                     , sqSquad :: (Maybe Squad)
                     }

type Position = (Int, Int)
type Field = [(Position, Square)]

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

generateField :: Field
generateField = undefined

moveSquad :: Field -> Position -> Position -> Field
moveSquad = undefined

runMyProj :: IO ()
runMyProj = putStrLn "This is a demo project!"
