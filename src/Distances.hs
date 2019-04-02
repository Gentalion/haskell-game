module Distances where

import Battle
import Hex (Position)
import qualified Hex
import Squad
import MovingSquad hiding (squad,rotation,position,animation)
import qualified MovingSquad (squad,rotation,position,animation)
import Data.Default

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

getMarchDistance' :: Int -> Battle -> Cell -> Cell -> Int
getMarchDistance' n b c1 c2 | (n > (fieldHeight b)) && (n > (fieldWidth b)) = error $ "Impossible #2 : " ++ outCell c1 ++ outCell c2
                            | any (\x -> position x == position c1) (getCellsOnMarchDistanceOrLess n b c2) = n
                            | otherwise = getMarchDistance' (n+1) b c1 c2

getPossibleMoves :: Battle -> Cell -> Int -> [Cell]
getPossibleMoves b c n = getCellsOnMarchDistanceOrLess n b c

-- get distance with obstacles
getMarchDistance :: Battle -> Cell -> Cell -> Int
getMarchDistance b c1 c2 = getMarchDistance' 1 b c1 c2

removeSelection :: Battle -> Battle
removeSelection b =
    let selected = maybe def id $ selection b
        b' = removeSelection' b
    in case (control $ maybe def id $ squad selected) of
        (NoControl) -> b' {otherCells = selected:(otherCells b')}
        (   Player) -> b' {allies = selected:(allies b')}
        (  EnemyAI) -> b' {enemies = selected:(enemies b')}

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

nextRouteStep :: Float -> Int -> Position -> Float -> [(Int, Position)] -> [Animation]
nextRouteStep _ 0 _ _ _ = []
nextRouteStep size n pos rot map = 
    let left = Hex.left pos
        right = Hex.right pos
        leftUp = Hex.leftUp pos
        rightDown = Hex.rightDown pos
        leftDown = Hex.leftDown pos
        rightUp = Hex.rightUp pos
    in case (filter (\(i,p) -> i == (n-1) && p == left) map) of
        (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size left) rot 540.0)++(nextRouteStep size (n-1) left 540.0 map)
        (  []) -> case (filter (\(i,p) -> i == (n-1) && p == right) map) of
            (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size right) rot 360.0)++(nextRouteStep size (n-1) right 360.0 map)
            (  []) -> case (filter (\(i,p) -> i == (n-1) && p == leftUp) map) of
                (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size leftUp) rot 480.0)++(nextRouteStep size (n-1) leftUp 480.0 map)
                (  []) -> case (filter (\(i,p) -> i == (n-1) && p == rightDown) map) of
                    (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size rightDown) rot 660.0)++(nextRouteStep size (n-1) rightDown 660.0 map)
                    (  []) -> case (filter (\(i,p) -> i == (n-1) && p == leftDown) map) of
                        (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size leftDown) rot 600.0)++(nextRouteStep size (n-1) leftDown 600.0 map)
                        (  []) -> case (filter (\(i,p) -> i == (n-1) && p == rightUp) map) of
                            (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size rightUp) rot 420.0)++(nextRouteStep size (n-1) rightUp 420.0 map)
                            (  []) -> error $ "Impossible #5 : " ++ show map

-- "reversed" in the name underlines that we seek the pass from second point for optimization
buildRouteReversed :: Battle -> Float -> Cell -> Float -> Cell -> [Animation]
buildRouteReversed b size c1 rot c2 =
    let dist = getMarchDistance b c1 c2
        map = buildMarchDistanceMap dist b c2
    in nextRouteStep size dist (position c1) rot (buildMarchDistanceMap dist b c2)

-- !!!previously selected cell should be here as c1
moveSquadAnimated :: Battle -> Float -> Cell -> Cell -> Battle
moveSquadAnimated b size c1 c2 =
    let c1Squad = maybe def id $ squad c1
        rot = rotation c1Squad
        b1 = (removeSelection' b)
    in b1 { otherCells = (c1 {squad = Nothing}):(otherCells b1) 
          , movingSquad = Just def { MovingSquad.squad = c1Squad
                                   , MovingSquad.rotation = rot
                                   , MovingSquad.position = Hex.evenrToPixel size $ position c1
                                   , MovingSquad.animation = buildRouteReversed b1 size c1 rot c2
                                   , MovingSquad.destination = position c2
                                   }
          }

-- turn for enemyAI
enemyAIturn :: Battle -> Battle
enemyAIturn = undefined

-- check whether player won, lost or is still playing
checkGameState :: Battle -> GameState
checkGameState b = case (allies b, enemies b, enemiesRemaining b) of
    ([], _,_) -> Lose
    ( _,[],0) -> Win
    ( _, _,_) -> Playing

member' :: Position -> [Position] -> Bool
member' pos posList = foldr (\y res -> res || pos == y) False posList

member :: Cell -> [Cell] -> Bool
member cell cellList = member' (position cell) $ map position cellList
