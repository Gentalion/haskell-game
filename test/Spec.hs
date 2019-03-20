import Battle

test :: (Int, Int) -> IO ()
test (x, y) = do 
    print (getStraightDistanceByPos (x, y) (hexLeft      (x, y)) )
    print (getStraightDistanceByPos (x, y) (hexLeftUp    (x, y)) )
    print (getStraightDistanceByPos (x, y) (hexLeftDown  (x, y)) )
    print (getStraightDistanceByPos (x, y) (hexRight     (x, y)) )
    print (getStraightDistanceByPos (x, y) (hexRightUp   (x, y)) )
    print (getStraightDistanceByPos (x, y) (hexRightDown (x, y)) )

main :: IO ()
main = do
    test (2, 2)
    test (2, 3)
    print (hexRightUp (2, 2)) 
    print (getStraightDistanceByPos (3, 3) (2, 4))
    print (getStraightDistanceByPos (2, 4) (3, 3))
    print (getStraightDistanceByPos (0,0) (6,6))
    print (getStraightDistanceByPos (6,6) (0,0))
    print (getStraightDistanceByPos (0,6) (6,0))
    print (getStraightDistanceByPos (6,0) (0,6))
    print (getStraightDistanceByPos (0,0) (4,6))
    print (getStraightDistanceByPos (4,6) (0,0))
    print (foldr1 max [getStraightDistanceByPos a b | a <- map position (generateHexField 7 7), b <- map position (generateHexField 7 7)])

