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
import Item
import Throughput
import Recipe

data Factory = Factory
  { inputs :: Map Item (Factory , Word) -- scaled factory
  , worker :: Recipe
  , workerCount :: Word -- output scale to make factory integral
  } deriving (Show)

factoryProduct :: Factory -> Item
factoryProduct f = product (worker f)

scaleFactory :: Word -> Factory -> Factory
scaleFactory s f = f { workerCount = s * workerCount f , inputs = second (s *) <$> inputs f }

instance HasThroughput Factory where
  outputPerSecond f = scaleThroughput (fromIntegral (workerCount f)) <$> outputPerSecond (worker f)
  inputPerSecond f = toThroughputList . collectThroughput $ do
    t <- inputPerSecond (worker f)
    case inputs f Map.!? item t of
      Just (f' , mult) -> scaleThroughput (fromIntegral mult) <$> inputPerSecond f'
      Nothing -> return $ scaleThroughput (fromIntegral (workerCount f)) t

{-
  bandwidth requirement for internal items
-}
internalThroughputPerSecond :: Factory -> [ Throughput Word Second ]
internalThroughputPerSecond f = toThroughputList . collectThroughput $ do
  (f' , k) <- toList $ inputs f
  scaleThroughput (fromIntegral k) <$> outputPerSecond' f' : internalThroughputPerSecond f'

{-
  finds the optimal ratio producing the item
  given a list of recipes
-}
-- XXX find shared subtrees
solveChain :: Item -> [ Recipe ] -> Maybe Factory
solveChain it context = do
  outputRecipe <- findProduct it context
  let factors = do
         t <- inputPerSecond outputRecipe
         f <- maybeToList $ solveChain (item t) context
         return (f , throughput t)
  let factor
        = foldl lcmRatio 1
        . fmap (\(f , ips) -> lcmRatio ips (throughput $ outputPerSecond' f) / ips)
        $ factors
  return $ Factory
    { inputs = fromList $ do
        (f , ips) <- factors
        let ops = throughput $ outputPerSecond' f
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
