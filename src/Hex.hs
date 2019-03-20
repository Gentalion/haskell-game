{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hex where

type Position = (Int, Int) -- first int stands for column, second for row

left :: Position -> Position
left (x, y) = (x - 1, y)

leftUp :: Position -> Position
leftUp (x, y) | (mod y 2) == 1 = (x - 1, y - 1)
                 | otherwise      = (x    , y - 1)

leftDown :: Position -> Position
leftDown (x, y) | (mod y 2) == 1 = (x - 1, y + 1)
                   | otherwise      = (x    , y + 1)

right :: Position -> Position
right (x, y) = (x + 1, y)

rightUp :: Position -> Position
rightUp (x, y) | (mod y 2) == 1 = (x    , y - 1)
                  | otherwise      = (x + 1, y - 1)

rightDown :: Position -> Position
rightDown (x, y) | (mod y 2) == 1 = (x    , y + 1)
                    | otherwise      = (x + 1, y + 1)

-- get distance without obstacles
getStraightDistance :: Position -> Position -> Int
getStraightDistance (x1,y1) (x2,y2) =
    let dx = x1 - x2
        dy = y1 - y2
    in case (abs dx, abs dy, signum dx, signum dy) of
        (0,n, _, _) -> n
        (n,0, _, _) -> n
        (_,_, 1, 1) -> 1 + getStraightDistance (leftUp    (x1,y1)) (x2,y2)
        (_,_,-1,-1) -> 1 + getStraightDistance (rightDown (x1,y1)) (x2,y2)
        (_,_,-1, 1) -> 1 + getStraightDistance (rightUp   (x1,y1)) (x2,y2)
        (_,_, 1,-1) -> 1 + getStraightDistance (leftDown  (x1,y1)) (x2,y2)
