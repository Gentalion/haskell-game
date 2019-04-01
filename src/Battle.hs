{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Battle where

import Hex (Position)
import qualified Hex
import Squad
import Data.Default
import MovingSquad hiding (squad,rotation,position,animation)
import qualified MovingSquad (squad,rotation,position,animation)
import Graphics.Gloss (Point)
import Const

data GameState = Win | Lose | Playing
data TerrainType = TerNothing | TerPlain | TerWater deriving Eq

instance Default TerrainType where
    def = TerNothing

data Cell = Cell { position :: Position
                 , terrain :: TerrainType
                 , squad :: Maybe Squad
                 }

instance Default Cell where
    def = Cell {position = (-2,-2), terrain = def, squad = Nothing} 

--type HexField = [Cell] -- we consider our field to be "even-r" hexagonal grid like it's shown here https://www.redblobgames.com/grids/hexagons/

type SquadPos = (Position, Float)
type Step = (SquadPos, SquadPos)
type Turn = [Step]

data Battle = Battle { otherCells :: [Cell]
                     , fieldHeight :: Int
                     , fieldWidth :: Int
                     , allies :: [Cell]
                     , enemies :: [Cell]
                     , enemiesRemaining :: Int
                     , selection :: Maybe Cell
                     , possibleMoves :: [Cell]
                     , previousTurns :: [Turn]
                     , movingSquad :: Maybe MovingSquad
                     }

instance Default Battle where
    def = Battle { otherCells = []
                 , fieldHeight = 0
                 , fieldWidth = 0
                 , allies = []
                 , enemies = []
                 , enemiesRemaining = 0
                 , selection = Nothing
                 , possibleMoves = []
                 , previousTurns = []
                 , movingSquad = Nothing
                 }

-- generate field with such height and width
generateHexField :: Int -> Int -> [Cell]
generateHexField width height = [def {position = (x,y), terrain = TerPlain} | x <- [0 .. width - 1], y <- [0 .. height - 1]]

getStraightDistance :: Cell -> Cell -> Int
getStraightDistance c1 c2 = Hex.getStraightDistance (position c1) (position c2)

-- get Cell from its position
getCellFromCellCollection :: [Cell] -> Position -> Cell
getCellFromCellCollection [] pos = ((def :: Cell) {position = pos})
getCellFromCellCollection (x:xs) pos | (pos == position x) = x
                               | otherwise = getCellFromCellCollection xs pos

field :: Battle -> [Cell]
field b = case (selection b) of
    (Nothing) -> (otherCells b)++(allies b)++(enemies b)
    ( Just c) -> c:(otherCells b)++(allies b)++(enemies b)++(possibleMoves b)


getCell :: Battle -> Position -> Cell
getCell b pos = getCellFromCellCollection (field b) pos

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

excludeCell :: [Cell] -> Cell -> [Cell]
excludeCell [] _ = []
excludeCell (x:xs) c | (position x) == (position c) = xs
                     | otherwise = x:(excludeCell xs c)

excludeCells :: [Cell] -> [Cell] -> [Cell]
excludeCells [] _ = []
excludeCells x [] = x
excludeCells x (y:ys) = excludeCells (excludeCell x y) ys

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
getCellsOnMarchDistanceOrLess' n b c = foldr (\x res -> (getCellsOnMarchDistanceOrLess' (n-1) b x)++res) [] (filter notObstacle $ getCellsOnStraightDistanceOrLess 1 b c)

getCellsOnMarchDistanceOrLess :: Int -> Battle -> Cell -> [Cell]
getCellsOnMarchDistanceOrLess n b c = clearFromDuplicates (getCellsOnMarchDistanceOrLess' n b c) []

outCell :: Cell -> String
outCell c = "("++show (fst $ position c)++","++show (snd $ position c)++") "

getMarchDistance' :: Int -> Battle -> Cell -> Cell -> Int
getMarchDistance' n b c1 c2 | (n > (fieldHeight b)) && (n > (fieldWidth b)) = error $ "Impossible #2 : " ++ outCell c1 ++ outCell c2
                            | any (\x -> position x == position c1) (getCellsOnMarchDistanceOrLess n b c2) = n
                            | otherwise = getMarchDistance' (n+1) b c1 c2

getPossibleMoves :: Battle -> Cell -> Int -> [Cell]
getPossibleMoves b@Battle{..} c n =
    let newField = otherCells++possibleMoves
    in getCellsOnMarchDistanceOrLess n b c

-- get distance with obstacles
getMarchDistance :: Battle -> Cell -> Cell -> Int
getMarchDistance b c1 c2 = getMarchDistance' 1 b c1 c2

modifyCellCollection :: [Cell] -> Cell -> [Cell]
modifyCellCollection [] c = [c]
modifyCellCollection (x:xs) c | (position x) == (position c) = c:xs
                              | otherwise = x:(modifyCellCollection xs c)

-- works correctly for Battle with no selection and empty possibleMoves i.e. for internal calls
modifyBattleWithCell :: Cell -> Battle -> Battle
modifyBattleWithCell c b =
    let pos = position c
        prevCellOccup = case (squad (getCell b pos)) of
            (Nothing) -> NoControl
            (Just ps) -> control ps
        newCellOccup = case (squad c) of
            (Nothing) -> NoControl
            (Just ns) -> control ns
    in case (prevCellOccup, newCellOccup) of
        (NoControl, NoControl) -> b {otherCells = modifyCellCollection (otherCells b) c}

        (NoControl,    Player) -> b {otherCells = excludeCell (otherCells b) c
                                    ,allies = c:(allies b)}

        (NoControl,   EnemyAI) -> b {otherCells = excludeCell (otherCells b) c
                                    ,enemies = c:(enemies b)}

        (   Player, NoControl) -> b {otherCells = c:(otherCells b)
                                    ,allies = excludeCell (allies b) c}

        (   Player,    Player) -> b {allies = modifyCellCollection (allies b) c}

        (   Player,   EnemyAI) -> b {allies = excludeCell (allies b) c
                                    ,enemies = c:(enemies b)}

        (  EnemyAI, NoControl) -> b {otherCells = c:(otherCells b)
                                    ,enemies = excludeCell (enemies b) c}

        (  EnemyAI,    Player) -> b {allies = c:(allies b)
                                    ,enemies = excludeCell (enemies b) c}

        (  EnemyAI,   EnemyAI) -> b {enemies = modifyCellCollection (enemies b) c}

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
        (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size left) rot 180.0)++(nextRouteStep size (n-1) left 180.0 map)
        (  []) -> case (filter (\(i,p) -> i == (n-1) && p == right) map) of
            (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size right) rot 0.0)++(nextRouteStep size (n-1) right 0.0 map)
            (  []) -> case (filter (\(i,p) -> i == (n-1) && p == leftUp) map) of
                (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size leftUp) rot 120.0)++(nextRouteStep size (n-1) leftUp 120.0 map)
                (  []) -> case (filter (\(i,p) -> i == (n-1) && p == rightDown) map) of
                    (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size rightDown) rot 300.0)++(nextRouteStep size (n-1) rightDown 300.0 map)
                    (  []) -> case (filter (\(i,p) -> i == (n-1) && p == leftDown) map) of
                        (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size leftDown) rot 240.0)++(nextRouteStep size (n-1) leftDown 240.0 map)
                        (  []) -> case (filter (\(i,p) -> i == (n-1) && p == rightUp) map) of
                            (x:xs) -> (generateMovement (Hex.evenrToPixel size pos) (Hex.evenrToPixel size rightUp) rot 60.0)++(nextRouteStep size (n-1) rightUp 60.0 map)
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

hexSize :: Battle -> Float
hexSize b = hexMaximumInWindowSize windowWidth windowHeight (fieldWidth b) (fieldHeight b)