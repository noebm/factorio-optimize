module Main where

import Lib
import Item

import Data.Foldable
import Data.Tree
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf
import Factory (factoryRecipes, solveRecipe)

printSolution :: String -> [ Recipe ] -> IO ()
printSolution itemName recipes = do
  let aux = recipeLookupMap recipes

  let Just factory = solveRecipe aux (Item itemName)
  let treeString = drawTree $ fmap recipeNameCount factory
  printf "Solution for %s:\n\n%s\n" itemName treeString

  printf "Using recipes:\n"
  mapM_ (putStrLn . prettyRecipe) $ factoryRecipes factory
  putStrLn ""

recipeLookupMap :: [ Recipe ] -> Map Item Recipe
recipeLookupMap recipes = Map.fromList $ do
  recipe <- reverse recipes
  (item, _count) <- toList $ products recipe
  return (item, recipe)

main :: IO ()
main = do
  printSolution "electronic-circuit" (toList recipesNoOre)

  printSolution "advanced-circuit" (toList recipesNoOre)

  printSolution "automation-science-pack" $ toList recipesNoOre

  printSolution "logistic-science-pack" $ toList recipesNoOre

  printSolution "processing-unit" $ toList recipesNoOre
