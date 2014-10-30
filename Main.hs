module Main where

import XO

h a = round (fromIntegral a * sqrt 3 / 2)

size = 400

triangle p1 p2 p3 = do
 drawLine (Line p1 p2)
 drawLine (Line p2 p3)
 drawLine (Line p3 p1)

mid (x1,y1) (x2,y2) = ((x1+x2)`div`2,(y1+y2)`div`2)

triangles 0 p1 p2 p3 = triangle p1 p2 p3
triangles n p1 p2 p3 = do
 triangles (n-1) p1 a2 a3
 triangles (n-1) a1 p2 a3
 triangles (n-1) a1 a2 p3
 where a1 = p2 `mid` p3
       a2 = p1 `mid` p3
       a3 = p1 `mid` p2

main = withCanvas (Rectangle 100 100 size (h size)) $ \(w,h) -> do
  triangles 7 (w`div`2,0) (0,h) (w,h)

