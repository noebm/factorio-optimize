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

printSolution :: String -> Map String Recipe -> IO ()
printSolution itemName recipes = do
  let ctx = snd <$> recipeLookupMap recipes

  let Just factory = solveRecipe ctx (Item itemName)
  let treeString = drawTree $ fmap recipeNameCount factory
  printf "Solution for %s:\n\n%s\n" itemName treeString

  printf "Using recipes:\n"
  mapM_ (putStrLn . prettyRecipe) $ factoryRecipes factory
  putStrLn ""

recipeLookupMap :: Map String Recipe -> Map Item (String, Recipe)
recipeLookupMap recipes = Map.fromList $ do
  (name, recipe) <- reverse $ Map.assocs recipes
  (item, _count) <- toList $ products recipe
  return (item, (name, recipe))

main :: IO ()
main = do
  printSolution "electronic-circuit" recipesNoOre

  printSolution "advanced-circuit" recipesNoOre

  printSolution "automation-science-pack" recipesNoOre

  printSolution "logistic-science-pack" recipesNoOre

  printSolution "processing-unit" recipesNoOre
