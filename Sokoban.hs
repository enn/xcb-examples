module Main where

import Data.Maybe
import Data.List

import X

charToBitmap c = fromJust (lookup c
 [ (' ', Blank)
 , ('#', Wall)
 , ('.', Goal)
 , ('@', Keeper)
 , ('+', KeeperOnGoal)
 , ('$', Box)
 , ('*', BoxOnGoal)
 ])

leve' = [ "   #########    ###"
        , "####  #    #    #@#"
        , "#  #$ #$## #  ### #"
        , "#...     # ####   #"
        , "#.#.##$  #    $$$##"
        , "#... # $ $ ##  $ ##"
        , "#.#. #$  # ## #   #"
        , "#... # $ # #  #   #"
        , "#.#.    ## #  #####"
        , "#... #   # # $ $ ##"
        , "### ##$# # #  $ $ #"
        , "##.*.  $   # $ $  #"
        , "#  *.#$  # # # # ##"
        , "# .*.      $     # "
        , "######$$ ######### "
        , "     #   #         "
        , "     #####         "
        ]
level (x,y) = charToBitmap $ (leve' !! y) !! x

rows = length leve'
cols = length (leve'!!0)
coords = [(x,y) | x <- [0..cols-1], y <- [0..rows-1]]

safe level (x,y) | 0 <= x && x < cols && 0 <= y && y < rows = Just $ level (x,y)
safe _ _ = Nothing

keeper level = find ((\s -> s==Keeper || s==KeeperOnGoal) . level) coords

nextSquare (x,y) DUp = (x,y-1)
nextSquare (x,y) DDown = (x,y+1)
nextSquare (x,y) DLeft = (x-1,y)
nextSquare (x,y) DRight = (x+1,y)

performMove dir level = do k <- keeper level
                           let s = nextSquare k dir
                           let t = nextSquare s dir
                           --
                           a <- safe level k
                           b <- safe level s
                           c <- safe level t
                           --
                           result <- performMove' [a,b,c]
                           --
                           return (\coord ->
                             if coord == k
                             then result !! 0
                             else if coord == s
                             then result !! 1
                             else if coord == t
                             then result !! 2
                             else level coord)

performMove' [a,b,c] = case [a,b,c] of
                         [Keeper,       Blank, x] -> Just [Blank, Keeper,       x]
                         [Keeper,       Goal,  x] -> Just [Blank, KeeperOnGoal, x]
                         [KeeperOnGoal, Blank, x] -> Just [Goal,  Keeper,       x]
                         [KeeperOnGoal, Goal,  x] -> Just [Goal,  KeeperOnGoal, x]
                         
                         [Keeper,       Box, Blank] -> Just [Blank, Keeper,     Box]
                         [KeeperOnGoal, Box, Blank] -> Just [Goal,  Keeper,     Box]
                         [Keeper,       BoxOnGoal, Blank] -> Just [Blank, KeeperOnGoal,     Box]
                         [KeeperOnGoal, BoxOnGoal, Blank] -> Just [Goal,  KeeperOnGoal,     Box]
                         [Keeper,       Box, Goal] -> Just [Blank, Keeper,     BoxOnGoal]
                         [KeeperOnGoal, Box, Goal] -> Just [Goal,  Keeper,     BoxOnGoal]
                         [Keeper,       BoxOnGoal, Goal] -> Just [Blank, KeeperOnGoal,     BoxOnGoal]
                         [KeeperOnGoal, BoxOnGoal, Goal] -> Just [Goal,  KeeperOnGoal,     BoxOnGoal]
                         
                         _ -> Nothing
performMove' _ = Nothing

main = withCanvas
  (Rectangle 100 100 (cols*20) (rows*20))
  level
  (\event level -> case event of
    KeyPress (Arrow dir) -> performMove dir level
    _ -> Nothing)
  (\(w,h) level -> do
    mapM_ (\(x,y) -> paintBitmap (level (x,y)) (x*20,y*20)) coords)



