{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RunGame where

import Graphics.Gloss
import Battle

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

runGame :: IO ()
runGame = putStrLn "This is a proud little game!"
