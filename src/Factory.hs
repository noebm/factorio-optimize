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
recipeTree :: Map Item Recipe -> Item -> Maybe (Tree Recipe)
recipeTree ctx it = unfoldTree go <$> ctx Map.!? it
  where
  go recipe =
    ( recipe
    , catMaybes [ ctx Map.!? item | (item, _) <- ingredients recipe ]
    )

-- | Correctly scales tree to fix ratios of intermediate steps.
scaleRecipeTree :: Tree Recipe -> Tree (Recipe, Ratio Word)
scaleRecipeTree = foldTree go where

  go :: Recipe -> [ Tree (Recipe, Ratio Word) ] -> Tree (Recipe, Ratio Word)
  go root children = Node (root, levelCoeff) scaledForest
    where
    recipeThroughputs (recipe, count) = scaleThroughput count <$> outputPerSecond recipe

    inputOutputPairs = toList . Map.intersectionWith (,) (inputPerSecond root)

    aux = inputOutputPairs . recipeThroughputs . rootLabel

    levelCoeff = foldl lcmRatio 1 $ do
      (ips, ops) <- aux =<< children
      return $ lcmRatio (throughput ips) (throughput ops) / throughput ips

    scaledForest = do
      t <- children
      (ips, ops) <- aux t
      let coeff = throughput ips / throughput ops * levelCoeff
      return $ fmap (second (coeff *)) t

solveRecipe :: Map Item Recipe -> Item -> Maybe (Tree (Recipe, Word))
solveRecipe ctx = fmap (integralCoefficients . scaleRecipeTree) . recipeTree ctx
  where integralCoefficients = fmap (second ratioToIntegral)

factoryRecipes :: Factory -> [ Recipe ]
factoryRecipes = toList . fmap fst
