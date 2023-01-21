module Recipes where

import System.IO.Unsafe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable

import Recipe
import Lua
import Item

recipes :: Map String Recipe
recipes = fst <$> unsafePerformIO loadLua

recipesNoOre :: Map String Recipe
recipesNoOre = Map.filterWithKey (\k _ -> not $ isOre k) recipes
  where
  isOre "copper-plate" = True
  isOre "iron-plate" = True
  isOre "steel-plate" = True
  isOre _ = False

recipeLookupMap :: Map String Recipe -> Map Item NamedRecipe
recipeLookupMap recipes = Map.fromList $ do
  (name, recipe) <- reverse $ Map.assocs recipes
  (item, _count) <- toList $ products recipe
  return (item, (name, recipe))
