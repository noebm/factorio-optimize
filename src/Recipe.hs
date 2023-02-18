{-# LANGUAGE DeriveLift #-}
module Recipe where

import Data.Foldable (find, toList)
import Data.List (intercalate)
import Text.Printf

import Data.Ratio

import Item
import Throughput

import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import Language.Haskell.TH.Syntax (Lift)

data Recipe = Recipe
  { ingredients :: [ (Item , Word) ]
  , products :: NonEmpty (Item, Word)
  , energy :: Ratio Word
  } deriving (Eq, Show, Lift)

instance HasThroughput Recipe where
  outputPerSecond recipe = Map.fromList $ do
    (item, count) <- toList $ products recipe
    return $ (,) item $ Throughput $ fromIntegral count / energy recipe
  inputPerSecond recipe = Map.fromList $ do
    (item, count) <- ingredients recipe
    return $ (,) item $ Throughput $ fromIntegral count / energy recipe

type NamedRecipe = (String, Recipe)

{-
  find a recipe that produces the item
-}
findProduct :: Foldable f => Item -> f Recipe -> Maybe Recipe
findProduct it = find (\recipe -> it `elem` fmap fst (products recipe))

prettyRecipe :: Recipe -> String
prettyRecipe recipe = printf "%s =>[%s] %s"
  (items (ingredients recipe)) (show $ energy recipe) (items (toList $ products recipe))
  where
  items list = intercalate ", " [ printf "%s x %d" (name it) c | (it, c) <- list ]

prettyNamedRecipe :: NamedRecipe -> String
prettyNamedRecipe (name, recipe) = printf "%s: %s" name $ prettyRecipe recipe

namedRecipeCountString :: (NamedRecipe, Word) -> String
namedRecipeCountString ((name, recipe), count) = printf "%s x %d" name count
