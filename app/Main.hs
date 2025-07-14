module Main where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree
import Factory (factoryRecipes, solveRecipe)
import Item
import Lib
import Text.Printf

solutionString :: String -> Tree (NamedRecipe, Word) -> String
solutionString itemName factory =
  unlines $
    [ printf "Solution for %s:" itemName,
      "",
      drawTree $ fmap namedRecipeCountString factory,
      "Using recipes:"
    ]
      ++ (prettyNamedRecipe <$> factoryRecipes factory)

printSolution :: String -> Map String Recipe -> IO ()
printSolution itemName recipes = do
  let ctx = recipeLookupMap recipes
  let Just factory = solveRecipe ctx (Item itemName)
  putStrLn $ solutionString itemName factory

main :: IO ()
main = do
  printSolution "electronic-circuit" recipesNoOre

  printSolution "advanced-circuit" recipesNoOre

  printSolution "automation-science-pack" recipesNoOre

  printSolution "logistic-science-pack" recipesNoOre

  printSolution "processing-unit" recipesNoOre
