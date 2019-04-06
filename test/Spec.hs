import Battle
import DrawBattle
import Squad
import Const
import InteractBattle
import Animate
import Data.Default
import Graphics.Gloss.Interface.IO.Game
import Distances

--printCellPos :: [Cell] -> IO ()
--printCellPos cells = print (foldr (\x res -> (position x):res) [] cells)

main :: IO ()
main = 
    let f = generateHexField 9 7
        c = def {position = (1,1), terrain = TerPlain}
        b = def {otherCells = f, fieldWidth = 9, fieldHeight = 7, targetsForEnemies = [(0,0)]}
        b1 = modifyBattleWithCell (c {position = (1,1), terrain = TerPlain, squad = Just (def {control = Player, rotation = 360.0, units = take 7 $ repeat def})}) b
        b2 = modifyBattleWithCell (c {position = (1,0), terrain = TerPlain, squad = Just (def {control = Enemy, rotation = 360.0, units = take 9 $ repeat def})}) b1
        b3 = modifyBattleWithCell (c {position = (0,2), terrain = TerPlain, squad = Just (def {control = Enemy, rotation = 360.0, units = take 9 $ repeat def})}) b2
        b4 = modifyBattleWithCell (c {position = (3,3), terrain = TerPlain, squad = Just (def {control = Enemy, rotation = 360.0, units = take 9 $ repeat def})}) b3
        --b3 = b2 {selection = Just $ position c, possibleMoves = (getPossibleMoves b2 c 2)}
    --in printCellPos (field b1)
    --in printCellPos (getCellsOnStraightDistanceOrLess 5 b1 c)
    --in printCellPos (getCellsOnMarchDistanceOrLess 2 b1 c)
    --in print (getMarchDistance b1 c (c {position = (3,3)}))
    --in drawGame b3
    in do print $ map position $ getNextInnerLayer b4 [] [getCell b4 (0,0)]
          playIO window bgColor framesPerSecond b4 drawBattle handleInput animateBattle
          