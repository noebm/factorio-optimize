{-# LANGUAGE TemplateHaskell #-}
module Recipes where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable

import Language.Haskell.TH.Syntax

import Recipe
import Lua
import Item

$(do
    recipeListE <- lift =<< runIO luaRecipeList
    let name = mkName "recipeList"
    return [ FunD name [Clause [] (NormalB recipeListE) []] ]
 )

recipes :: Map String Recipe
recipes = fst <$> Map.fromList recipeList

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
