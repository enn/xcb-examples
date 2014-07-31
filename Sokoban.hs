module Main where

import Data.Maybe

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

level = [ "   #########    ###"
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


main = withCanvas (Rectangle 100 100 (cols*20) (rows*20)) $ \(w,h) -> do
  mapM_ (\(i,j) -> paintBitmap (charToBitmap ((level !! j) !! i)) (i*20,j*20)) [(i,j) | i <- [0..cols-1], j <- [0..rows-1] ]
 where rows = length level
       cols = length (level!!0)


