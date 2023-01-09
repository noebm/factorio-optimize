module Main where

import Lib
import Item

import Data.Foldable
import Data.Tree
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf
import Factory (factoryRecipes)

branchName = intercalate "," . fmap (name . fst) . toList . products
branchNameCount (recipe, count) = printf "%s x %d" (branchName recipe) count

printSolution :: String -> [ Recipe ] -> IO ()
printSolution itemName recipes = do
  let aux = recipeLookupMap recipes

  let Just tree = recipeTree aux (Item itemName)
  let factory = scaleRecipeTree tree
    -- optimalFactory (Item itemName) (toList recipesNoOre)
  printf "Solution for %s:\n" itemName

  putStrLn $ drawTree $ fmap branchNameCount factory
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

{-
  let itemToRecipe = recipeLookupMap $ toList recipesNoOre

  let Just circuitTree1 = recipeTree itemToRecipe (Item "electronic-circuit")
  let Just circuitTree2 = recipeTree itemToRecipe (Item "advanced-circuit")
  let Just tree3 = recipeTree itemToRecipe (Item "copper-cable")

  let circuitTree1' = scaleRecipeTree circuitTree1
  let circuitTree2' = scaleRecipeTree circuitTree2
  let tree3' = scaleRecipeTree tree3

  putStrLn $ drawTree $ fmap branchName tree3
  putStrLn $ drawTree $ fmap branchNameCount tree3'

  putStrLn $ drawTree $ fmap branchName circuitTree1
  putStrLn $ drawTree $ fmap branchNameCount circuitTree1'

  putStrLn $ drawTree $ fmap branchName circuitTree2
  putStrLn $ drawTree $ fmap branchNameCount circuitTree2'

  -- mapM_ (putStrLn . drawTree . branchName)
  --   $ recipeTree itemToRecipe (Item "electronic-circuit")

  -- let aux :: Tree Recipe -> [[String]]
  --     aux = take 5 . levels . branchName

  -- mapM_ (putStrLn . unlines . fmap unwords . aux)
  -- mapM_ (putStrLn . drawTree . branchName)
  --   $ recipeTree itemToRecipe (Item "advanced-circuit")
-}
