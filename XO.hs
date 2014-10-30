{-# LANGUAGE ForeignFunctionInterface #-}

-- ghc Main x.c -lxcb

module XO
 ( Rectangle(..), Line(..)
 , X
 , withCanvas
 , fillRect, drawLine
 ) where

import Data.IORef
import Foreign.C.Types
import Foreign.Ptr(FunPtr, freeHaskellFunPtr)

data Rectangle = Rectangle Int Int Int Int
data Line = Line (Int,Int) (Int,Int)

newtype X a = X { io :: Int -> Int -> IO a }

instance Monad X where
 return x = X (\win gc -> return x)
 m >>= f = X (\win gc -> io m win gc >>= \e -> io (f e) win gc)

foreign import ccall "x_initialize" io_x_initialize :: IO ()
foreign import ccall "x_create_window" io_x_create_window :: Int -> Int -> Int -> Int -> IO Int
foreign import ccall "x_create_graphics_context" io_x_create_graphics_context :: Int -> IO Int
foreign import ccall "x_handle_events" io_x_handle_events :: FunPtr (Int -> IO ()) -> FunPtr (Int -> Int -> Int -> IO ()) -> IO ()
foreign import ccall "x_fill_rectangle" io_x_fill_rectangle :: Int -> Int -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "x_draw_line" io_x_draw_line :: Int -> Int -> Int -> Int -> Int -> Int -> IO ()

-- http://www.haskell.org/haskellwiki/GHC/Using_the_FFI
-- a "wrapper" import gives a factory for converting a Haskell function to a foreign function pointer
foreign import ccall "wrapper"
  wrap :: (Int -> IO ()) -> IO (FunPtr (Int -> IO ()))
foreign import ccall "wrapper"
  wrap2 :: (Int -> Int -> Int -> IO ()) -> IO (FunPtr (Int -> Int -> Int -> IO ()))

withCanvas :: Rectangle -> ((Int,Int) -> X ()) -> IO ()
withCanvas (Rectangle x y w h) paint = do
  ref <- newIORef (w,h)
  io_x_initialize
  --
  win <- io_x_create_window x y w h
  gc <- io_x_create_graphics_context win
  --
  ev <- wrap (eventHandler ref win gc paint)
  cfg <- wrap2 (configureHandler ref)
  --
  io_x_handle_events ev cfg
  --
  freeHaskellFunPtr ev
  freeHaskellFunPtr cfg

eventHandler ref win gc paint = \i -> do
 (w,h) <- readIORef ref
 io (paint (w,h)) win gc
configureHandler ref = \win' w h -> do
 writeIORef ref (w,h)

fillRect :: Rectangle -> X ()
fillRect (Rectangle x y w h) = X (\win gc -> io_x_fill_rectangle win gc x y w h)

drawLine :: Line -> X ()
drawLine (Line (x1,y1) (x2,y2)) = X (\win gc -> io_x_draw_line win gc x1 y1 x2 y2)
