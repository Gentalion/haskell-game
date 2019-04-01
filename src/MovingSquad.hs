{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MovingSquad ( animateMovingSquad
                   , MovingSquad(..)
                   , Rotation
                   , Movement
                   , Animation
                   , generateMovement
                   , getSmth
                   , getSmth') where

import Squad (Squad)
import qualified Squad as Squad
import Hex (Position)
import Graphics.Gloss (Point)
import Data.Default
import Data.Fixed
import Const

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
                               , destination :: Position
                               }

instance Default MovingSquad where
  def = MovingSquad {squad = def, position = (-2.0,-2.0), rotation = 0.0, animation = [], destination = (-2,-2)} 

animateMovingSquad :: Float -> MovingSquad -> MovingSquad
animateMovingSquad f ms = case (animation ms, head $ animation ms) of
    ([],             _) -> ms
    ( _,Left  rotation) -> animateRotation f rotation (ms {animation = tail $ animation ms})
    ( _,Right movement) -> animateMovement f movement (ms {animation = tail $ animation ms})

anglePlus :: Float -> Float -> Float
anglePlus x y | x + y >= 0 = x + y
              | otherwise = 360.0 + (x + y)

angleComparison :: Float -> Float -> Bool
angleComparison a b | b > 180.0 = a <= b
                    | otherwise = a >= b

--animateRotation :: Float -> Rotation -> MovingSquad -> MovingSquad
--animateRotation f rot ms = ms {animation = (Left rot):(animation ms)}

animateRotation :: Float -> Rotation -> MovingSquad -> MovingSquad
animateRotation f rot@Rotation{..} ms | angleComparison (anglePlus (rotation ms) (f * delta)) tmax = ms {rotation = tmax}
                                      | otherwise = ms {rotation = anglePlus (rotation ms) (f * delta), animation = (Left rot):(animation ms)}


(+++) :: Point -> Point -> Point
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pointComparison :: Point -> Point -> Point -> Bool
pointComparison (x1,y1) (dx,dy) (x2,y2) = case (dx > 0, dy > 0) of
    ( True,  True) -> x1+dx >= x2 && y1+dy >= y2
    (False, False) -> x1+dx <= x2 && y1+dy <= y2
    ( True, False) -> x1+dx >= x2 && y1+dy <= y2
    (False,  True) -> x1+dx <= x2 && y1+dy >= y2

deltaPoint :: Point -> Point -> Float -> Point
deltaPoint (x1, y1) (x2, y2) divider = ((x2 - x1) / divider, (y2 - y1) / divider)

multiplyFloatPoint :: Float -> Point -> Point
multiplyFloatPoint f (px,py) = (f * px, f * py)

--animateMovement :: Float -> Movement -> MovingSquad -> MovingSquad
--animateMovement f mov ms = ms {animation = (Right mov):(animation ms)}

animateMovement :: Float -> Movement -> MovingSquad -> MovingSquad
animateMovement f mov@Movement{..} ms | pointComparison (position ms) (multiplyFloatPoint f delta) tmax = ms {position = tmax}
                                      | otherwise = ms {position = ((position ms) +++ (multiplyFloatPoint f delta)), animation = (Right mov):(animation ms)}

minimalRotation :: Float -> Float -> Rotation
minimalRotation angle1 angle2 = 
    case (abs (angle2 - angle1) <= 180.0) of
        ( True) -> Rotation {tmax = angle2, delta = (angle2 - angle1) / rotationAnimationTime}
        (False) -> Rotation {tmax = angle2, delta = - (360.0 - angle2 + angle1) / rotationAnimationTime}

generateMovement :: Point -> Point -> Float -> Float -> [Animation]
generateMovement pos destinationPos rot destinationRot = ((Left $ minimalRotation rot destinationRot)
                                                :(Right $ Movement {tmax = destinationPos, delta = deltaPoint pos destinationPos $ movementAnimationTime})
                                                :[])

getSmth :: [Animation] -> String
getSmth [] = ""
getSmth ((Left x):xs) = show $ (tmax :: Rotation -> Float) x
getSmth ((Right x):xs) = show $ (tmax :: Movement -> Point) x

getSmth' :: MovingSquad -> [Animation] -> String
getSmth' ms [] = ""
getSmth' ms ((Left x):xs) = show $ (delta :: Rotation -> Float) x
getSmth' ms ((Right x):xs) = show $ (position ms)+++((delta :: Movement -> Point) x)