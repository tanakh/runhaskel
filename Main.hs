{-# Language ForeignFunctionInterface #-}

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import UI.HSCurses.Curses
import System.Posix.Signals

img =
  [ "__   __ __  __  ____   ___      _________________________________________  "
  , "||   || ||  || ||  || ||__      Hugs 98: Based on the Haskel 98 standard   "
  , "||___|| ||__|| ||__||  __||     Copyright (c) 1994-2005                    "
  , "||---||         ___||           World Wide Web: http://haskel.org/hugs     "
  , "||   ||                         Bugs: http://hackage.haskel.org/trac/hugs  "
  , "||   || Version: September 2006 _________________________________________  "
  , "                                                                           "
  , "Haskel 98 mode: Restart with command line option -98 to enable extensions  "
  ]

mvAddStr :: Int -> Int -> [Char] -> IO ()
mvAddStr y x str =
  forM_ (zip str [x..]) $ \(c, cx) -> do
    mvAddCh y cx (fromIntegral $ fromEnum c)

main :: IO ()
main = bracket_ initCurses endWin $ do
  installHandler sigINT Ignore Nothing
  
  (lnes, cols) <- scrSize
  let y = lnes `div` 2 - length img `div` 2
  
  forM_ [cols, cols-1 .. - (length $ img !! 0)] $ \x -> do
    forM_ [0 .. length img - 1] $ \i -> do
      mvAddStr (y + i) x (img !! i)
    refresh
    threadDelay $ 20000
