import Battle
import DrawBattle
import Squad

--printCellPos :: [Cell] -> IO ()
--printCellPos cells = print (foldr (\x res -> (position x):res) [] cells)

main :: IO ()
main = 
    let f = generateHexField 5 9
        b = Battle {field = f, fieldHeight = 5, fieldWidth = 9, allies = [], enemies = [], enemiesRemaining = 0, selection = Nothing, possibleMoves = [], previousTurns = []}
        c = Cell {position = (0,0), terrain = TerPlain, squad = Nothing}
        b1 = modifyBattleWithCell b (c {position = (1,1), terrain = TerWater, squad = Just (emptySquad {control = Player, rotation = 60.0, units = take 7 (repeat emptyUnit)})})
    --in printCellPos (field b1)
    --in printCellPos (getCellsOnStraightDistanceOrLess 5 b1 c)
    --in printCellPos (getCellsOnMarchDistanceOrLess 2 b1 c)
    --in print (getMarchDistance b1 c (c {position = (3,3)}))
    in drawGame b1