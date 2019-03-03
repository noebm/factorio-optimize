module Recipe
where

import Prelude hiding (product)
import Control.Arrow (first)
import Data.Foldable (find)

import Data.Ratio

newtype Item = Item
  { name :: String }
  deriving (Eq, Ord, Show)

data Recipe = Recipe
  { ingredients :: [ (Word , Item) ]
  , product :: Item
  , productCount :: Word
  , craftingTime :: Ratio Word
  } deriving (Eq, Show)

ingredientsPerSecond :: Recipe -> [ (Ratio Word, Item) ]
ingredientsPerSecond b = first ((/ t) . fromIntegral) <$> ingredients b
  where t = craftingTime b

ingredientPerSecond :: Recipe -> Item -> Maybe (Ratio Word)
ingredientPerSecond b i = fmap fst $ find ((== i) . snd) $ ingredientsPerSecond b

ingredientsItems :: Recipe -> [ Item ]
ingredientsItems b = snd <$> ingredients b

productsPerSecond :: Recipe -> Ratio Word
productsPerSecond b = fromIntegral (productCount b) / craftingTime b

{-
  find a building that produces the item
-}
findProduct :: Item -> [ Recipe ] -> Maybe Recipe
findProduct item = find (\ building -> product building == item)
