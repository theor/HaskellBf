module Lib (
    module Op
  , module State
  , someFunc
) where

import Op
import State

someFunc :: IO ()
someFunc = putStrLn "someFunc"
