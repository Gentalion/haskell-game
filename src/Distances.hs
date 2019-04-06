module Distances where

import Battle
import Hex (Position)
import qualified Hex
import Squad
import MovingSquad hiding (squad,rotation,position,animation)
import qualified MovingSquad (squad,rotation,position,animation)
import Data.Default
import Data.List
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ

getStraightDistance :: Cell -> Cell -> Int
getStraightDistance c1 c2 = Hex.getStraightDistance (position c1) (position c2)

cellLeft :: Battle -> Cell -> Cell
cellLeft b c = getCell b $ Hex.left (position c)

cellLeftUp :: Battle -> Cell -> Cell
cellLeftUp b c = getCell b $ Hex.leftUp (position c)

cellLeftDown :: Battle -> Cell -> Cell
cellLeftDown b c = getCell b $ Hex.leftDown (position c)

cellRight :: Battle -> Cell -> Cell
cellRight b c = getCell b $ Hex.right (position c)

cellRightUp :: Battle -> Cell -> Cell
cellRightUp b c = getCell b $ Hex.rightUp (position c)

cellRightDown :: Battle -> Cell -> Cell
cellRightDown b c = getCell b $ Hex.rightDown (position c)

legitCells :: Battle -> [Cell] -> [Cell]
legitCells b cells =
    let h = (fieldHeight b)
        w = (fieldWidth  b)
    in [c | c <- cells, (fst $ position c) >=  0, (fst $ position c) <  w, (snd $ position c) >=  0, (snd $ position c) <  h]

getNeighbors :: Battle -> Cell -> [Cell]
getNeighbors b c = legitCells b $ (cellLeft b c):(cellLeftUp b c):(cellLeftDown b c):(cellRight b c):(cellRightUp b c):(cellRightDown b c):[]

-- second [Cell] is used as accumulating parameter
clearFromDuplicates :: [Cell] -> [Cell] -> [Cell]
clearFromDuplicates [] res = res
clearFromDuplicates (x:xs) [] = clearFromDuplicates xs (x:[])
clearFromDuplicates (x:xs) res | not (foldr (\y res -> res || position x == position y) False res) = clearFromDuplicates xs (x:res)
                               | otherwise = clearFromDuplicates xs res

-- get all other cells on distance x
getCellsOnStraightDistanceOrLess' :: Int -> Battle -> Cell -> [Cell]
getCellsOnStraightDistanceOrLess' 1 b c = getNeighbors b c
getCellsOnStraightDistanceOrLess' n b c = foldr1 (++) $ map (getCellsOnStraightDistanceOrLess (n-1) b) (getNeighbors b c)

getCellsOnStraightDistanceOrLess :: Int -> Battle -> Cell -> [Cell]
getCellsOnStraightDistanceOrLess n b c = excludeCell (clearFromDuplicates (getCellsOnStraightDistanceOrLess' n b c) []) c

-- check whether terrain is obstacle or there is a squad
notObstacle :: Cell -> Bool
notObstacle c = case (terrain c, squad c) of
    (_       , Just _) -> False
    (TerWater,      _) -> False
    (       _,      _) -> True

getCellsOnMarchDistanceOrLess' :: Int -> Battle -> Cell -> [Cell]
getCellsOnMarchDistanceOrLess' 1 b c = filter notObstacle (getCellsOnStraightDistanceOrLess' 1 b c)
getCellsOnMarchDistanceOrLess' n b c = foldr (\x res -> x:(getCellsOnMarchDistanceOrLess' (n-1) b x)++res) [] (filter notObstacle $ getCellsOnStraightDistanceOrLess 1 b c)

getCellsOnMarchDistanceOrLess :: Int -> Battle -> Cell -> [Cell]
getCellsOnMarchDistanceOrLess n b c = excludeCell (clearFromDuplicates (getCellsOnMarchDistanceOrLess' n b c) []) c

outCell :: Cell -> String
outCell c = "("++show (fst $ position c)++","++show (snd $ position c)++") "

getMarchDistanceBFS :: Int -> Battle -> Cell -> Cell -> [Cell] -> Int
getMarchDistanceBFS n b c1 c2 oldCOMD | null (excludeCells newCOMD oldCOMD) = 9999
                                    | (n > (fieldHeight b) + (fieldWidth b)) = error $ "Impossible #2 : " ++ outCell c1 ++ outCell c2
                                    | any (\x -> position x == position c2) (getCellsOnMarchDistanceOrLess n b c1) = n
                                    | otherwise = getMarchDistanceBFS (n+1) b c1 c2 newCOMD
    where newCOMD = (getCellsOnMarchDistanceOrLess n b c1)

getPossibleMoves :: Battle -> Cell -> Int -> [Cell]
getPossibleMoves b c n = getCellsOnMarchDistanceOrLess n b c

getNextInnerLayer :: Battle -> [Cell] -> [Cell] -> [Cell]
getNextInnerLayer b interier inner = 
    let nextInnerPlus = filter notObstacle $ foldr (++) [] $ map (getNeighbors b) inner
        prev = interier++inner
    in foldr (\x res -> if ((member x res) || (member x prev)) then res else x:res) [] nextInnerPlus

intersectCellsBool :: [Cell] -> [Cell] -> Bool
intersectCellsBool m1 m2 = foldr (\x res -> (member x m1) || res) False m2

getMarchDistanceBFS2 :: Battle -> (Int,[Cell],[Cell]) -> (Int,[Cell],[Cell]) -> Int
getMarchDistanceBFS2 b (d1,oldCOMD1,inner1) (d2,oldCOMD2,inner2) =
    case ((d1 + d2 > (fieldWidth b) + (fieldHeight b)) || null inner1 || null inner2, d1 > d2) of
        (True ,    _) -> 9999
        (False, True) -> 
            let newCOMD2 = inner2++oldCOMD2
                newInner2 = getNextInnerLayer b oldCOMD2 inner2
            in case (intersectCellsBool inner1 newInner2) of 
                ( True) -> d1 + d2 + 1
                (False) -> getMarchDistanceBFS2 b (d1,oldCOMD2,inner1) (d2+1,newCOMD2,newInner2)
        (False,False) ->
            let newCOMD1 = inner1++oldCOMD1
                newInner1 = getNextInnerLayer b oldCOMD1 inner1
            in case (intersectCellsBool newInner1 inner2) of
                ( True) -> d1 + d2 + 1
                (False) -> getMarchDistanceBFS2 b (d1+1,newCOMD1,newInner1) (d2,oldCOMD2,inner2)

getMarchDistanceAStar :: Battle -> Cell -> MinPQueue Int Cell -> Int
getMarchDistanceAStar b destCell pq = 
    let (curDist,curCell) = PQ.findMin pq
        realDistFromStart = curDist - getStraightDistance curCell destCell
        curNeighbors = filter notObstacle $ getNeighbors b curCell
        curNeighborsWithDist = map (\x -> (getStraightDistance x destCell,x)) curNeighbors
    in case (realDistFromStart > 4, any (\(dist,cell) -> dist == 0) curNeighborsWithDist) of
        (    _, True) -> realDistFromStart + 1
        (False,False) -> getMarchDistanceAStar b destCell $ foldr (\(dist,cell) res -> PQ.insert (dist + realDistFromStart + 1) cell res) pq curNeighborsWithDist
        ( True,    _) -> 9999

--getMarchDistance :: Battle -> Cell -> Cell -> Int
--getMarchDistance b c1 c2 | (position c1 /= position c2) = getMarchDistanceAStar b c2 $ PQ.insert (getStraightDistance c1 c2) c1 PQ.empty
--                         | otherwise = 0
getMarchDistance :: Battle -> Cell -> Cell -> Int
getMarchDistance b c1 c2 | (position c1 /= position c2) = getMarchDistanceBFS2 b (0,[],[c1]) (0,[],[c2])
                         | otherwise = 0
-- get distance with obstacles
--getMarchDistance :: Battle -> Cell -> Cell -> Int
--getMarchDistance b c1 c2 | (position c1 /= position c2) = getMarchDistanceBFS 1 b c1 c2 []
--                         | otherwise = 0

removeSelection :: Battle -> Battle
removeSelection b =
    let selected = maybe def id $ selection b
        b' = removeSelection' b
    in case (control $ maybe def id $ squad selected) of
        (NoControl) -> b' {otherCells = selected:(otherCells b')}
        (   Player) -> b' {allies = selected:(allies b')}
        (    Enemy) -> b' {enemies = selected:(enemies b')}

removeSelection' :: Battle -> Battle
removeSelection' b = b {otherCells = (otherCells b)++(possibleMoves b), selection = Nothing, possibleMoves = []}

moveSquad :: Battle -> Cell -> Cell -> Battle
moveSquad b c1 c2 = 
    let c1Squad = squad c1
        c2Squad = squad c2
    in case (c1Squad, c2Squad) of
        (Nothing, Nothing) -> error "Impossible #3"
        ( Just x, Nothing) -> modifyBattleWithCell (c1 {squad = Nothing}) $ modifyBattleWithCell (c2 {squad = Just x}) $ removeSelection' b
        (Nothing,  Just y) -> modifyBattleWithCell (c1 {squad = Just y}) $ modifyBattleWithCell (c2 {squad = Nothing}) $ removeSelection' b
        ( Just x,  Just y) -> error "Impossible #4"

buildMarchDistanceMap'' :: Int -> Int -> Battle -> (Int,Cell) -> [(Int, Cell)]
buildMarchDistanceMap'' 1 index b (i,c) = (i,c) : [(index, x) | x <- filter notObstacle (getCellsOnStraightDistanceOrLess' 1 b c)]
buildMarchDistanceMap'' n index b (i,c) = foldr (\(i, c) res -> (buildMarchDistanceMap'' (n-1) (index+1) b (i,c))++res) 
                                           [] 
                                           (buildMarchDistanceMap'' 1 index b (i,c))

buildMarchDistanceMap' :: Int -> Battle -> Cell -> [(Int, Position)]
buildMarchDistanceMap' n b c = map (\(i,c) -> (i, position c)) $ buildMarchDistanceMap'' n 1 b (0,c)

leaveLessSingle :: (Int, Position) -> [(Int, Position)] -> [(Int, Position)]
leaveLessSingle x [] = [x]
leaveLessSingle (xi,xp) ((yi,yp):ys) =
    case (xp == yp, xi >= yi) of
        (False,    _) -> (yi,yp):(leaveLessSingle (xi,xp) ys)
        (True,  True) -> (yi,yp):ys
        (True, False) -> (xi,xp):ys

leaveLess :: [(Int, Position)] -> [(Int, Position)] -> [(Int, Position)]
leaveLess [] ys = ys
leaveLess (x:xs) ys = leaveLess xs (leaveLessSingle x ys)

buildMarchDistanceMap :: Int -> Battle -> Cell -> [(Int, Position)]
buildMarchDistanceMap n b c = leaveLess (buildMarchDistanceMap' n b c) []

nextRouteStep :: Float -> Int -> Int -> Position -> Float -> [(Int, Position)] -> [MSAnimation]
nextRouteStep _ _ 0 _ _ _ = []
nextRouteStep _ 0 _ _ _ _ = []
nextRouteStep size n steps pos rot map = 
    let left = Hex.left pos
        right = Hex.right pos
        leftUp = Hex.leftUp pos
        rightDown = Hex.rightDown pos
        leftDown = Hex.leftDown pos
        rightUp = Hex.rightUp pos
    in case (filter (\(i,p) -> i == (n-1) && p == left) map) of
        (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size left) rot 540.0)++(nextRouteStep size (n-1) (steps-1) left 540.0 map)
        (  []) -> case (filter (\(i,p) -> i == (n-1) && p == right) map) of
            (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size right) rot 360.0)++(nextRouteStep size (n-1) (steps-1) right 360.0 map)
            (  []) -> case (filter (\(i,p) -> i == (n-1) && p == leftUp) map) of
                (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size leftUp) rot 480.0)++(nextRouteStep size (n-1) (steps-1) leftUp 480.0 map)
                (  []) -> case (filter (\(i,p) -> i == (n-1) && p == rightDown) map) of
                    (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size rightDown) rot 660.0)++(nextRouteStep size (n-1) (steps-1) rightDown 660.0 map)
                    (  []) -> case (filter (\(i,p) -> i == (n-1) && p == leftDown) map) of
                        (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size leftDown) rot 600.0)++(nextRouteStep size (n-1) (steps-1) leftDown 600.0 map)
                        (  []) -> case (filter (\(i,p) -> i == (n-1) && p == rightUp) map) of
                            (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size rightUp) rot 420.0)++(nextRouteStep size (n-1) (steps-1) rightUp 420.0 map)
                            (  []) -> error $ "Impossible #5 : " ++ show map

-- "reversed" in the name underlines that we seek the pass from second point for optimization
buildRouteReversed :: Battle -> Float -> Cell -> Float -> Cell -> [MSAnimation]
buildRouteReversed b size c1 rot c2 =
    let dist = getMarchDistance b c1 c2
        map = buildMarchDistanceMap dist b c2
    in nextRouteStep size dist dist (position c1) rot map

-- !!!previously selected cell should be here as c1
moveSquadAnimated :: Battle -> Float -> Cell -> Cell -> Battle
moveSquadAnimated b size c1 c2 =
    let c1Squad = maybe def id $ squad c1
        rot = rotation c1Squad
        b1 = (removeSelection' b)
    in b1 { otherCells = (c1 {squad = Nothing}):(otherCells b1) 
          , animation = (def { MovingSquad.squad = c1Squad
                            , MovingSquad.rotation = rot
                            , MovingSquad.position = Hex.evenrToPixel size $ position c1
                            , MovingSquad.animation = buildRouteReversed b1 size c1 rot c2
                            , MovingSquad.destination = position c2
                            }):[]
          }

-- check whether player won, lost or is still playing
checkGameState :: Battle -> GameState
checkGameState b = case (allies b, enemies b) of
    ([], _) -> Lose
    ( _,[]) -> Win
    ( _, _) -> Playing

member' :: Position -> [Position] -> Bool
member' pos posList = foldr (\y res -> res || pos == y) False posList

member :: Cell -> [Cell] -> Bool
member cell cellList = member' (position cell) $ map position cellList
