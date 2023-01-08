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
  , energy :: Word -- ^ 2 energy correspond to 1 second.
  } deriving (Eq, Show)

instance HasThroughput Recipe where
  outputPerSecond recipe = pure $ Throughput (product recipe) $ fromIntegral (productCount recipe) / t
    where t = fromIntegral $ energy recipe
  inputPerSecond recipe = uncurry Throughput . second ((/ t) . fromIntegral) <$> ingredients recipe
    where t = fromIntegral $ energy recipe

{-
  find a recipe that produces the item
-}
findProduct :: Foldable f => Item -> f Recipe -> Maybe Recipe
findProduct it = find ((== it) . product)

