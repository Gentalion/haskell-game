{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Squad where

import Hex (Position)
import Data.Default

data ModifierType = ModAddWhite | ModAddGreen | ModMultiply | ModWound deriving Eq
data Control = Player | EnemyAI | NoControl deriving Eq

instance Default Control where
    def = NoControl

data Modifier = Modifier { modName :: String
                         , modType :: ModifierType
                         , modValue :: Float
                         }

data Unit = Unit { name :: String
                 , basePower :: Float
                 , mods :: [Modifier]
                 }

instance Default Unit where
    def = Unit {name = "", basePower = 0.0, mods = []}

data Squad = Squad { name :: String
                   , control :: Control
                   , rotation :: Float
                   , steps :: Int
                   , maxMoveDist :: Int
                   , attackDist :: Int
                   , units :: [Unit]
                   , mods :: [Modifier]
                   }

instance Default Squad where
    def = Squad {name = "", control = def, rotation = 0.0, steps = 0, maxMoveDist = 0, attackDist = 0, units = [], mods = []}

-- filter all modifiers with ModifierType equal ModAddWhite and sum them
allMAW :: [Modifier] -> Float
allMAW mods = foldr (+) 0.0 $ map modValue $ filter (\cur -> modType cur == ModAddWhite) mods

-- filter all modifiers with ModifierType equal ModAddGreen and sum them
allMAG :: [Modifier] -> Float
allMAG mods = foldr (+) 0.0 $ map modValue $ filter (\cur -> modType cur == ModAddGreen) mods

-- filter all modifiers with ModifierType equal ModMultiply and multiply them
allMM :: [Modifier] -> Float
allMM mods = foldr (*) 1.0 $ map modValue $ filter (\cur -> modType cur == ModMultiply) mods

allWounds :: [Modifier] -> Float
allWounds mods = foldr (+) 0.0 $ map modValue $ filter (\cur -> modType cur == ModWound) mods

-- calculate unit power with modifiers
unitRealPower :: Unit -> Float
unitRealPower Unit{..} = (basePower + (allMAW mods)) * (allMM mods) + (allMAG mods) -- power = (own power + white power) * power multiplier + green power

-- calculate squad power
squadPower :: Squad -> Float
squadPower s = (foldr (\x res -> res + (unitRealPower x)) 0.0 (units s)) - (allWounds $ mods (s :: Squad))