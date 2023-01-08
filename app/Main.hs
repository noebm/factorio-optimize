module Main where

import Lib
import Item

import Data.Foldable

main :: IO ()
main = do
  let Just factory = optimalFactory (Item "electronic-circuit") (toList recipes)
  putStrLn $ simplFactoryShow factory

