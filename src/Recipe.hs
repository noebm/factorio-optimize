module Recipe
where

import Prelude hiding (product)
import Control.Arrow (second)
import Data.Foldable (find)

import Data.Ratio

import Item
import Throughput

data Recipe = Recipe
  { ingredients :: [ (Item , Word) ]
  , product :: Item
  , productCount :: Word
  , craftingTime :: Ratio Word
  } deriving (Eq, Show)

instance HasThroughput Recipe where
  outputPerSecond recipe = pure $ Throughput (product recipe) $ fromIntegral (productCount recipe) / t
    where t = craftingTime recipe
  inputPerSecond recipe = uncurry Throughput . second ((/ t) . fromIntegral) <$> ingredients recipe
    where t = craftingTime recipe

{-
  find a recipe that produces the item
-}
findProduct :: Foldable f => Item -> f Recipe -> Maybe Recipe
findProduct it = find ((== it) . product)

