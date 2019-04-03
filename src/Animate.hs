module Animate where

import Battle
import MovingSquad (MovingSquad)
import qualified MovingSquad
import Squad
import Combat
import EnemyAI

animateBattle :: Float -> Battle -> IO Battle
animateBattle f b = case (animation b, movingEnemies b) of
    (  [],[]) -> return $ b
    (  [], l) -> return $ enemyAIturn b
    (anim,_) -> 
        case (foldr1 (++) $ map MovingSquad.animation anim) of
            ([]) -> return $ checkEnemyTargets $ foldr modifyBattleWithCell (b {animation = []}) $ map (finishAnimation b) anim
            ( _) -> return $ b {animation = map (MovingSquad.animateMovingSquad f) anim}

finishAnimation :: Battle -> MovingSquad -> Cell
finishAnimation b ms = (getCell b (MovingSquad.destination ms)) {squad = Just $ squad {rotation = MovingSquad.rotation ms}}
    where squad = MovingSquad.squad ms