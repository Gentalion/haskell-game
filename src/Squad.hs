{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Squad where

data ModifierType = ModAddWhite | ModAddGreen | ModMultiply | ModWound deriving Eq
data Control = Player | EnemyAI | NoControl

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

-- filter all modifiers with ModifierType equal ModAddWhite and sum them
allMAW :: [Modifier] -> Float
allMAW mods = foldr (+) 0.0 (map modValue (filter (\cur -> modType cur == ModAddWhite) mods))

-- filter all modifiers with ModifierType equal ModAddGreen and sum them
allMAG :: [Modifier] -> Float
allMAG mods = foldr (+) 0.0 (map modValue (filter (\cur -> modType cur == ModAddGreen) mods))

-- filter all modifiers with ModifierType equal ModMultiply and multiply them
allMM :: [Modifier] -> Float
allMM mods = foldr (*) 1.0 (map modValue (filter (\cur -> modType cur == ModMultiply) mods))

allWounds :: [Modifier] -> Float
allWounds mods = foldr (+) 0.0 (map modValue (filter (\cur -> modType cur == ModWound) mods))

-- calculate unit power with modifiers
unitRealPower :: Unit -> Float
unitRealPower Unit{..} = (basePower + (allMAW mods)) * (allMM mods) + (allMAG mods) -- power = (own power + white power) * power multiplier + green power

-- calculate squad power
squadPower :: Squad -> Float
squadPower s = (foldr (\x res -> res + (unitRealPower x)) 0.0 (members s)) - (allWounds (mods (s :: Squad)))