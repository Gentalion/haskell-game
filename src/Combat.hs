module Combat where

import Battle

attackAnybody :: Battle -> Cell -> Int -> IO Battle
attackAnybody b c 0 = return b
attackAnybody b c n = undefined