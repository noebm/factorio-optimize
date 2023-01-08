module Objects where

import System.IO.Unsafe
import Data.Map
import qualified Data.Map as Map

import Recipe
import Lua

recipes :: Map String Recipe
recipes = fst <$> unsafePerformIO loadLua

recipesNoOre :: Map String Recipe
recipesNoOre = Map.filterWithKey (\k _ -> not $ isOre k) recipes
  where
  isOre "copper-plate" = True
  isOre "iron-plate" = True
  isOre "steel-plate" = True
  isOre _ = False
