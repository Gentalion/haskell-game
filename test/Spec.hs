import Battle

test :: (Int, Int) -> IO ()
test (x, y) = do 
    print (getStraightDistance (x, y) (hexLeft      (x, y)) )
    print (getStraightDistance (x, y) (hexLeftUp    (x, y)) )
    print (getStraightDistance (x, y) (hexLeftDown  (x, y)) )
    print (getStraightDistance (x, y) (hexRight     (x, y)) )
    print (getStraightDistance (x, y) (hexRightUp   (x, y)) )
    print (getStraightDistance (x, y) (hexRightDown (x, y)) )

main :: IO ()
main = do
    test (2, 2)
    test (2, 3)
    print (hexRightUp (2, 2)) 
    print (getStraightDistance (3, 3) (2, 4))
    print (getStraightDistance (2, 4) (3, 3))
    print (getStraightDistance (0,0) (6,6))
    print (getStraightDistance (6,6) (0,0))
    print (getStraightDistance (0,6) (6,0))
    print (getStraightDistance (6,0) (0,6))
    print (getStraightDistance (0,0) (4,6))
    print (getStraightDistance (4,6) (0,0))
    print (foldr1 max [getStraightDistance a b | a <- map position (generateHexField 7 7), b <- map position (generateHexField 7 7)])

