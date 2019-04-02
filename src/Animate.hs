module Animate where

import Battle
import qualified MovingSquad
import Squad
import Combat

animateBattle :: Float -> Battle -> IO Battle
animateBattle f b = case (movingSquad b) of
    (Nothing) -> return $ b
    (Just ms) -> 
        let squad = MovingSquad.squad ms
            cell = (getCell b (MovingSquad.destination ms)) {squad = Just $ squad {rotation = MovingSquad.rotation ms}}
        in case (MovingSquad.animation ms, control squad) of
            ([],  Player) -> attackAnybody (modifyBattleWithCell cell (b {movingSquad = Nothing})) cell (attackDist squad)
            ([], EnemyAI) -> return $ b {movingSquad = Nothing}
            ([],       _) -> return $ b {movingSquad = Nothing}
            ( _,       _) -> return $ b {movingSquad = Just $ MovingSquad.animateMovingSquad f ms}