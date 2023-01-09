module Main where

import Lib
import Item

import Data.Foldable
import Text.Printf
import Factory (factoryRecipes)
import qualified Data.Map as Map

printSolution :: String -> [ Recipe ] -> IO ()
printSolution itemName recipes = do
  let Just factory = optimalFactory (Item itemName) (toList recipesNoOre)
  printf "Solution for %s:\n" itemName
  putStrLn $ simplFactoryShow factory
  printf "Using recipes:\n"
  mapM_ (putStrLn . prettyRecipe) $ factoryRecipes factory
  putStrLn ""


main :: IO ()
main = do
  printSolution "electronic-circuit" (toList recipesNoOre)

  printSolution "advanced-circuit" (toList recipesNoOre)

  printSolution "automation-science-pack" $ toList recipesNoOre

  printSolution "logistic-science-pack" $ toList recipesNoOre
