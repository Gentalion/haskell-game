import Battle

printCellPos :: [Cell] -> IO ()
printCellPos cells = print (foldr (\x res -> (position x):res) [] cells)

main :: IO ()
main = 
    let f = generateHexField 7 7
        b = Battle {field = f, fieldHeight = 7, fieldWidth = 7, allies = [], enemies = [], enemiesRemaining = 0, selection = Nothing, previousTurns = []}
        c = Cell {position = (0,0), terrain = TerWater, squad = Nothing}
        b1 = modifyBattleWithCell b (c {position = (1,1)})
    --in printCellPos (getCellsOnStraightDistanceOrLess 2 b c)
    in printCellPos (getCellsOnMarchDistanceOrLess 2 b1 c)

