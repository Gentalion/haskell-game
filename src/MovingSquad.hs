{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MovingSquad (animateMovingSquad, MovingSquad) where

import Squad (Squad)
import qualified Squad as Squad
import Graphics.Gloss (Point)

data Rotation = Rotation { delta :: Float
                         , tmax :: Float
                         }

data Movement = Movement { delta :: Point
                         , tmax :: Point
                         }

type Animation = Either Rotation Movement

data MovingSquad = MovingSquad { squad :: Squad
                               , position :: Point
                               , rotation :: Float
                               , animation :: [Animation]
                               }

animateMovingSquad :: MovingSquad -> MovingSquad
animateMovingSquad ms = case (animation ms, head $ animation ms) of
    ([],             _) -> ms
    ( _,Left  rotation) -> animateRotation rotation ms
    ( _,Right movement) -> animateMovement movement ms

animateRotation :: Rotation -> MovingSquad -> MovingSquad
animateRotation rot@Rotation{..} ms | (rotation ms) + delta > tmax = ms {rotation = tmax}
                                    | otherwise = ms {rotation = (rotation ms) + delta, animation = (Left rot):(animation ms)}


(+++) :: Point -> Point -> Point
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(>>>) :: Point -> Point -> Bool
(>>>) (x1, y1) (x2, y2) = x1 > x2 || y1 > y2

animateMovement :: Movement -> MovingSquad -> MovingSquad
animateMovement mov@Movement{..} ms | (position ms) +++ delta >>> tmax = ms {position = tmax}
                                    | otherwise = ms {position = (position ms) +++ delta, animation = (Right mov):(animation ms)}