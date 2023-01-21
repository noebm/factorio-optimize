module Factory
where

import Data.Foldable (toList)
import Control.Arrow (second)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Tree
import Data.Ratio

import Ratio
import Item
import Throughput
import Recipe

type Factory = Tree (Recipe, Word)

-- | Generates tree of recipes (as inputs for the parent recipe).
-- Each key in the Map should be an output of the corresponding recipe.
--
-- Recipes with multiple outputs might require additional merging if the parent
-- or a collection of parents use more than one output.
recipeTree :: Map Item NamedRecipe -> Item -> Maybe (Tree NamedRecipe)
recipeTree ctx it = unfoldTree go <$> ctx Map.!? it
  where
  go (name, recipe) =
    ( (name, recipe)
    , catMaybes [ ctx Map.!? item | (item, _) <- ingredients recipe ]
    )

-- | Correctly scales tree to fix ratios of intermediate steps.
scaleRecipeTree :: Tree NamedRecipe -> Tree (NamedRecipe, Ratio Word)
scaleRecipeTree = foldTree go where

  go :: NamedRecipe -> [ Tree (NamedRecipe, Ratio Word) ] -> Tree (NamedRecipe, Ratio Word)
  go root children = Node (root, levelCoeff) scaledForest
    where
    rootRecipe = snd root
    recipeThroughputs ((_ , recipe), count) = scaleThroughput count <$> outputPerSecond recipe

    inputOutputPairs = toList . Map.intersectionWith (,) (inputPerSecond rootRecipe)

    aux = inputOutputPairs . recipeThroughputs . rootLabel

    levelCoeff = foldl lcmRatio 1 $ do
      (ips, ops) <- aux =<< children
      return $ lcmRatio (throughput ips) (throughput ops) / throughput ips

    scaledForest = do
      t <- children
      (ips, ops) <- aux t
      let coeff = throughput ips / throughput ops * levelCoeff
      return $ fmap (second (coeff *)) t

solveRecipe :: Map Item NamedRecipe -> Item -> Maybe (Tree (NamedRecipe, Word))
solveRecipe ctx = fmap (integralCoefficients . scaleRecipeTree) . recipeTree ctx
  where integralCoefficients = fmap (second ratioToIntegral)

factoryRecipes :: Tree (a, b) -> [a]
factoryRecipes = toList . fmap fst
