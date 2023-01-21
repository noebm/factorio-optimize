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

solutionString :: String -> Factory -> String
solutionString itemName factory = unlines $
    [ printf "Solution for %s:" itemName
    , ""
    , drawTree $ fmap recipeNameCount factory
    , "Using recipes:"
    ]
    ++ (prettyRecipe <$> factoryRecipes factory)

printSolution :: String -> Map String Recipe -> IO ()
printSolution itemName recipes = do
  let ctx = snd <$> recipeLookupMap recipes
  let Just factory = solveRecipe ctx (Item itemName)
  putStrLn $ solutionString itemName factory

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
