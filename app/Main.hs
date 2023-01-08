module Main where

import Lib
import Item

import Data.Foldable
import Text.Printf
import Factory (factoryRecipes)

main :: IO ()
main = do
  let name = "electronic-circuit"
  let Just factory = optimalFactory (Item name) (toList recipes)
  printf "Solution for %s:\n" name
  putStrLn $ simplFactoryShow factory
  putStrLn ""

  printf "Using recipes:\n"
  mapM_ (putStrLn . prettyRecipe) $ factoryRecipes factory

