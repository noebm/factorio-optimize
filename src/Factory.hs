module Factory
where

import Prelude hiding (product)
import Data.Foldable (toList)
import Control.Arrow (first, second)
import Control.Monad
import Data.List (intercalate, nub)
import Data.Maybe (maybeToList, fromJust, catMaybes)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty

import Data.Ratio
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

scaleRecipeTree :: Tree Recipe -> Tree (Recipe, Word)
scaleRecipeTree = foldTree go where

  go :: Recipe -> [ Tree (Recipe, Word) ] -> Tree (Recipe, Word)
  go root children = Node
    (root, ratioToIntegral scale)
    scaledSubtree
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

factoryRecipes :: Factory -> [ Recipe ]
factoryRecipes = toList . fmap fst
