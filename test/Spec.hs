import Battle
import DrawBattle
import Squad

--printCellPos :: [Cell] -> IO ()
--printCellPos cells = print (foldr (\x res -> (position x):res) [] cells)

main :: IO ()
main = 
    let f = generateHexField 5 9
        c = Cell {position = (1,1), terrain = TerPlain, squad = Nothing}
        b = Battle {field = f, fieldHeight = 5, fieldWidth = 9, allies = [], enemies = [], enemiesRemaining = 0, selection = Nothing, possibleMoves = [], previousTurns = []}
        b1 = modifyBattleWithCell (c {position = (1,1), terrain = TerWater, squad = Just (emptySquad {control = Player, rotation = 0.0, units = take 7 (repeat emptyUnit)})}) b
        b2 = modifyBattleWithCell (c {position = (2,2), terrain = TerWater, squad = Just (emptySquad {control = EnemyAI, rotation = 0.0, units = take 9 (repeat emptyUnit)})}) b1
        b3 = b2 {possibleMoves = (getPossibleMoves b2 c 2)}
    --in printCellPos (field b1)
    --in printCellPos (getCellsOnStraightDistanceOrLess 5 b1 c)
    --in printCellPos (getCellsOnMarchDistanceOrLess 2 b1 c)
    --in print (getMarchDistance b1 c (c {position = (3,3)}))
    in drawGame b3