{-# LANGUAGE ForeignFunctionInterface #-}

-- ghc Main x.c -lxcb -lxcb-image

module X
 ( Rectangle(..), Line(..), Bitmap(..)
 , X, Direction(..), Key(..), Event(..)
 , withCanvas
 , fillRect, drawLine, paintBitmap
 ) where

import Data.IORef
import Foreign.C.Types
import Foreign.Ptr(FunPtr, freeHaskellFunPtr)

data Rectangle = Rectangle Int Int Int Int
data Line = Line (Int,Int) (Int,Int)
data Bitmap =
   Blank
 | Wall
 | Goal
 | Keeper
 | KeeperOnGoal
 | Box
 | BoxOnGoal
 deriving Eq

bitmapToId Blank = 0
bitmapToId Wall = 1
bitmapToId Goal = 2
bitmapToId Keeper = 3
bitmapToId KeeperOnGoal = 4
bitmapToId Box = 5
bitmapToId BoxOnGoal = 6

newtype X a = X { io :: Int -> Int -> IO a }

instance Monad X where
 return x = X (\win gc -> return x)
 m >>= f = X (\win gc -> io m win gc >>= \e -> io (f e) win gc)

data Direction = DUp | DDown | DLeft | DRight deriving Show
data Key = Arrow Direction
data Event = KeyPress Key

foreign import ccall "x_initialize" io_x_initialize :: IO ()
foreign import ccall "x_create_window" io_x_create_window :: Int -> Int -> Int -> Int -> IO Int
foreign import ccall "x_create_graphics_context" io_x_create_graphics_context :: Int -> IO Int
foreign import ccall "x_handle_events" io_x_handle_events :: FunPtr (Int -> IO ()) -> FunPtr (Int -> Int -> Int -> IO ()) -> FunPtr (Int -> IO ()) -> IO ()
foreign import ccall "x_fill_rectangle" io_x_fill_rectangle :: Int -> Int -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "x_draw_line" io_x_draw_line :: Int -> Int -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "x_paint_pixmap" io_x_paint_pixmap :: Int -> Int -> Int -> Int -> Int -> IO ()

-- http://www.haskell.org/haskellwiki/GHC/Using_the_FFI
-- a "wrapper" import gives a factory for converting a Haskell function to a foreign function pointer
foreign import ccall "wrapper"
  wrap :: (Int -> IO ()) -> IO (FunPtr (Int -> IO ()))
foreign import ccall "wrapper"
  wrap2 :: (Int -> Int -> Int -> IO ()) -> IO (FunPtr (Int -> Int -> Int -> IO ()))

withCanvas :: Rectangle -> state -> (Event -> state -> Maybe state) -> ((Int,Int) -> state -> X ()) -> IO ()
withCanvas (Rectangle x y w h) initial event paint = do
  ref <- newIORef ((w,h),initial)
  io_x_initialize
  --
  win <- io_x_create_window x y w h
  gc <- io_x_create_graphics_context win
  --
  ev <- wrap (eventHandler ref win gc paint)
  cfg <- wrap2 (configureHandler ref)
  key <- wrap (keyPressHandler ref win gc paint event)
  --
  io_x_handle_events ev cfg key
  --
  freeHaskellFunPtr ev
  freeHaskellFunPtr cfg
  freeHaskellFunPtr key

eventHandler ref win gc paint = \i -> do
 ((w,h),state) <- readIORef ref
 io (paint (w,h) state) win gc
keyPressHandler ref win gc paint event = \i -> do
 let key = case i of
             0 -> DUp ; 1 -> DDown ; 2 -> DLeft ; 3 -> DRight
 ((w,h),state) <- readIORef ref
 case event (KeyPress (Arrow key)) state of
   Nothing -> return ()
   Just state' -> do
     writeIORef ref ((w,h),state')
     io (paint (w,h) state') win gc
configureHandler ref = \win' w h -> do
 (_,state) <- readIORef ref
 writeIORef ref ((w,h),state)

fillRect :: Rectangle -> X ()
fillRect (Rectangle x y w h) = X (\win gc -> io_x_fill_rectangle win gc x y w h)

drawLine :: Line -> X ()
drawLine (Line (x1,y1) (x2,y2)) = X (\win gc -> io_x_draw_line win gc x1 y1 x2 y2)

paintBitmap :: Bitmap -> (Int,Int) -> X ()
paintBitmap b (x,y) = X (\win gc -> io_x_paint_pixmap win gc x y (bitmapToId b))

