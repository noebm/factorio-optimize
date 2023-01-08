module Recipe
where

import Prelude hiding (product)
import Control.Arrow (second)
import Data.Foldable (find, toList)
import Data.List (intercalate)
import Text.Printf

import Data.Ratio

import Item
import Throughput

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

data Recipe = Recipe
  { ingredients :: [ (Item , Word) ]
  , products :: NonEmpty (Item, Word)
  , energy :: Word -- ^ 2 energy correspond to 1 second.
  } deriving (Eq, Show)

instance HasThroughput Recipe where
  outputPerSecond recipe = do
    (item, count) <- products recipe
    return $ Throughput item $ count % energy recipe
  inputPerSecond recipe = do
    (item, count) <- ingredients recipe
    return $ Throughput item $ count % energy recipe

{-
  find a recipe that produces the item
-}
findProduct :: Foldable f => Item -> f Recipe -> Maybe Recipe
findProduct it = find (\recipe -> it `elem` fmap fst (products recipe))

prettyRecipe :: Recipe -> String
prettyRecipe recipe = printf "%s =>[%d] %s" (items (ingredients recipe)) (energy recipe) (items (toList $ products recipe))
  where
  items list = intercalate ", " [ printf "%s x %d" (name it) c | (it, c) <- list ]
