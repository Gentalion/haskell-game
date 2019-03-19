import MyProj

main :: IO ()
main = do
    print (getStraightDistance (0,0) (6,6))
    print (getStraightDistance (6,6) (0,0))
    print (getStraightDistance (0,6) (6,0))
    print (getStraightDistance (6,0) (0,6))
    print (getStraightDistance (0,0) (4,6))
    print (getStraightDistance (4,6) (0,0))
