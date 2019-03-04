module Factory
where

import Prelude hiding (product)
import Data.Foldable (toList)
import Control.Arrow (second)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Data.Map (Map , fromList)
import qualified Data.Map as Map

import Data.Ratio

import Ratio
import Recipe

data Factory = Factory
  { inputs :: Map Item (Factory , Word) -- scaled factory
  , worker :: Recipe
  , workerCount :: Word -- output scale to make factory integral
  } deriving (Show)

factoryOutputPerSecond :: Factory -> Ratio Word
factoryOutputPerSecond f = productsPerSecond (worker f) * fromIntegral (workerCount f)

factoryProduct :: Factory -> Item
factoryProduct f = product (worker f)

scaleFactory :: Word -> Factory -> Factory
scaleFactory s f = f { workerCount = s * workerCount f , inputs = second (s *) <$> inputs f }

{-
  bandwidth requirement for external items
-}
requiredInputs :: Factory -> [ (Item, Ratio Word) ]
requiredInputs f = Map.toList $ Map.unionsWith (+) $ fmap (fromList . return) $ do
  (item , itemsPerSecond) <- ingredientsPerSecond (worker f)
  case inputs f Map.!? item of
    Just (f' , mult) -> second (fromIntegral mult *) <$> requiredInputs f'
    Nothing -> return (item, itemsPerSecond * fromIntegral (workerCount f))

{-
  bandwidth requirement including intermediate steps
-}
requiredThroughput :: Factory -> [ (Item, Ratio Word) ]
requiredThroughput f = Map.toList $ Map.unionsWith (+) $ fmap (fromList . return) $ do
  x <- ingredientsPerSecond (worker f)
  (:) (second (fromIntegral (workerCount f) *) x) $ do
    (f', mult) <- maybeToList . (inputs f Map.!?) $ fst x
    second (fromIntegral mult *) <$> requiredThroughput f'

{-
  finds the optimal ratio producing the item
  given a list of recipes
-}
-- XXX find shared subtrees
solveChain :: Item -> [ Recipe ] -> Maybe Factory
solveChain item context = do
  outputRecipe <- findProduct item context
  let factors = do
         (item' , ips) <- ingredientsPerSecond outputRecipe
         f <- maybeToList $ solveChain item' context
         return (f , ips)
  let factor
        = foldl lcmRatio 1
        . fmap (\(f , ips) -> lcmRatio ips (factoryOutputPerSecond f) / ips)
        $ factors
  return $ Factory
    { inputs = fromList $ do
        (f , ips) <- factors
        let ops = factoryOutputPerSecond f
        return $ (,) (factoryProduct f) (f , ratioToIntegral (ips / ops * factor))
    , worker = outputRecipe
    , workerCount = ratioToIntegral factor
    }

simplFactoryShow :: Factory -> String
simplFactoryShow f = unlines (aux f) where
  aux :: Factory -> [ String ]
  aux f' =
    [ intercalate "\n" $ (replicate 2 ' ' ++) <$> aux (scaleFactory scale f'')
    | (f'' , scale) <- toList (inputs f') ]
    ++
    [ name (product (worker f')) ++ " x " ++ show (workerCount f')]
