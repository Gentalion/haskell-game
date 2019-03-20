import Battle

printCellPos :: [Cell] -> IO ()
printCellPos cells = print (foldr (\x res -> (position x):res) [] cells)

main :: IO ()
main = 
    let f = generateHexField 7 7
        b = Battle {field = f, fieldHeight = 7, fieldWidth = 7, allies = [], enemies = [], selection = Nothing, previousTurns = []}
        c = Cell {position = (2,2), terrain = TerNothing, squad = Nothing}
    in printCellPos (getCellsOnStraightDistanceOrLess 2 b c)

