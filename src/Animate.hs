module Animate where

import Battle
import qualified MovingSquad
import Squad

animateBattle :: Float -> Battle -> Battle
animateBattle f b = case (movingSquad b) of
    (Nothing) -> b
    (Just ms) -> case (MovingSquad.animation ms, control $ MovingSquad.squad ms) of
        ([],  Player) -> modifyBattleWithCell ((getCell b (MovingSquad.destination ms)) {squad = Just $ (MovingSquad.squad ms) {rotation = MovingSquad.rotation ms}})
                                              (b { movingSquad = Nothing})
        ([], EnemyAI) -> b {movingSquad = Nothing}
        ([],       _) -> b {movingSquad = Nothing}
        ( _,       _) -> b {movingSquad = Just $ MovingSquad.animateMovingSquad f ms}