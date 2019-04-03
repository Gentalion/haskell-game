module EnemyAI where

import Battle
import Distances
import qualified MovingSquad as MS
import Squad
import Const
import Hex (Position, evenrToPixel)
import Graphics.Gloss (Point)
import Data.Default

singleEnemyTurnToKnownTarget :: Battle -> Float -> Cell -> Cell -> Battle
singleEnemyTurnToKnownTarget b size c1 c2 =
    let dist = getMarchDistance b c1 c2
        map = buildMarchDistanceMap dist b c2
        c1squad = maybe def id $ squad c1
        rot = rotation c1squad
        --movementAnimation = buildRouteReversed b size c1 rot c2
        movementAnimation = nextRouteStep size dist (maxSteps c1squad) (position c1) rot map
    in case (dist) of
        (9999) -> b
        (   _) -> b { otherCells = (c1 {squad = Nothing}):(otherCells b)
                    , enemies = excludeCell (enemies b) c1
                    , movingSquad = Just def { MS.squad = c1squad
                                             , MS.position = evenrToPixel (hexSize b) $ position c1
                                             , MS.rotation = rot
                                             , MS.animation = movementAnimation
                                             , MS.destination = MS.destinationFromAnimation movementAnimation size
                                             }
                    }

singleEnemyTurn :: Battle -> Cell -> Battle
singleEnemyTurn b c = singleEnemyTurnToKnownTarget b (hexSize b) c 
                              $ snd $ foldr (\x y -> if fst x > fst y then y else x) (9999,def) $ map (\x -> (getMarchDistance b c x, x))
                              $ map (getCell b) (targetsForEnemies b)

--singleEnemyTurn :: Battle -> Cell -> Battle
--singleEnemyTurn b c = moveSquadAnimated b (hexSize b) c $ cellRightDown b c

-- turn for enemyAI
enemyAIturn :: Battle -> Battle
enemyAIturn b =
    let movEnemies = movingEnemies b
    in (singleEnemyTurn b (head movEnemies)) {movingEnemies = tail movEnemies}

checkEnemyTargets :: Battle -> Battle
checkEnemyTargets b =
    let hasCome = filter (\x -> (maybe 0 (\x -> 1) $ squad x) /= 0) $ map (getCell b) (targetsForEnemies b)
    in foldr (\x res -> modifyBattleWithCell (x {squad = Nothing}) res) b hasCome