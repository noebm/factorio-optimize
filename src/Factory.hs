module Factory
where

import Data.Foldable (toList)
import Control.Arrow (second)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Tree

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
scaleRecipeTree :: Tree Recipe -> Tree (Recipe, Word)
scaleRecipeTree = foldTree go where

  go :: Recipe -> [ Tree (Recipe, Word) ] -> Tree (Recipe, Word)
  go root children = Node (root, ratioToIntegral scale) scaledSubtree
    where
    childrenOutputs = do
      subtree <- children
      let (recipe, count) = rootLabel subtree
      return (subtree, scaleThroughput (fromIntegral count) <$> outputPerSecond recipe)

    paired = do
      (subtree, opsList) <- childrenOutputs
      (ips, ops) <- toList $ Map.intersectionWith (,) (inputPerSecond root) opsList
      return (subtree, (ips, ops))

    scale = foldl lcmRatio 1 $ do
      (_, (ips, ops)) <- paired
      return $ lcmRatio (throughput ips) (throughput ops) / throughput ips

    scaledSubtree = do
      (t, (ips, ops)) <- paired
      let f count = ratioToIntegral (throughput ips / throughput ops * scale) * count
      return $ fmap (second f) t

solveRecipe :: Map Item Recipe -> Item -> Maybe (Tree (Recipe, Word))
solveRecipe ctx = fmap scaleRecipeTree . recipeTree ctx

factoryRecipes :: Factory -> [ Recipe ]
factoryRecipes = toList . fmap fst
