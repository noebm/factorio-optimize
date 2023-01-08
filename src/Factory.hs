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

scaleFactory :: Word -> Factory -> Factory
scaleFactory s f = f { workerCount = s * workerCount f , inputs = second (s *) <$> inputs f }

instance HasThroughput Factory where
  outputPerSecond f = scaleThroughput (fromIntegral (workerCount f)) <$> outputPerSecond (worker f)
  inputPerSecond f = collectThroughput $ do
    t <- inputPerSecond (worker f)
    case inputs f Map.!? item t of
      Just (f' , mult) -> scaleThroughput (fromIntegral mult) <$> inputPerSecond f'
      Nothing -> return $ scaleThroughput (fromIntegral (workerCount f)) t

-- | Throughputs of all subfactories
internalThroughputPerSecond :: Factory -> [ Throughput Word Second ]
internalThroughputPerSecond = collectThroughput . go
  where
  go f = do
    (f' , k) <- toList $ inputs f
    scaleThroughput (fromIntegral k) <$> toList (outputPerSecond f') ++ go f'

{- |
  Finds the optimal ratio producing the item given a list of 'Recipe's.
-}
-- XXX find shared subtrees
optimalFactory :: Item -> [ Recipe ] -> Maybe Factory
optimalFactory it context = do
  outputRecipe <- findProduct it context
  let optimizedInputs = do
         t <- inputPerSecond outputRecipe
         -- nonexistent solution should cause failure!
         f <- maybeToList $ optimalFactory (item t) context
         return (f , throughput t)
  let inputAdjustment
        = foldl lcmRatio 1
        . fmap (\(f , ips) -> lcmRatio ips (throughput $ outputPerSecond' f) / ips)
        $ optimizedInputs
  return $ Factory
    { inputs = fromList $ do
        (f , ips) <- optimizedInputs
        (it, ops) <- (\t -> (item t, throughput t)) <$> toList (outputPerSecond f)
        return $ (,) it (f , ratioToIntegral (ips / ops * inputAdjustment))
    , worker = outputRecipe
    , workerCount = ratioToIntegral inputAdjustment
    }

simplFactoryShow :: Factory -> String
simplFactoryShow f = unlines (aux f) where
  aux :: Factory -> [ String ]
  aux f' =
    [ intercalate "\n" $ (replicate 2 ' ' ++) <$> aux (scaleFactory scale f'')
    | (f'' , scale) <- toList (inputs f') ]
    ++
    [ unwords (toList (fmap (name . fst) (products (worker f')))) ++ " x " ++ show (workerCount f')]
