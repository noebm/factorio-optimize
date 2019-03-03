module Lib
  ( module Recipe
  , module Factory
  , module Objects
  , someFunc
  )
where

import Recipe
import Factory
import Objects

someFunc :: IO ()
someFunc = putStrLn "someFunc"
