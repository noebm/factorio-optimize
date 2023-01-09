module Factory
where

import Prelude hiding (product)
import Data.Foldable (toList)
import Control.Arrow (first, second)
import Control.Monad
import Data.List (intercalate, nub)
import Data.Maybe (maybeToList)
import Data.Map (Map , fromList)
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.Tree

import Data.Ratio

import Ratio
import Item
import Throughput
import Recipe

data Factory a = Factory
  { inputs :: Map Item (Factory a)
  , worker :: a
  , workerCount :: Word -- output scale to make factory integral
  } deriving (Show)

factoryItems :: Factory a -> [ Item ]
factoryItems = nub . go where

  go factory = Map.keys (inputs factory) ++ concatMap go (Map.elems $ inputs factory)

factoryRecipes :: Factory Recipe -> [ Recipe ]
factoryRecipes = nub . go where

  go factory = worker factory : concatMap go (Map.elems $ inputs factory)

scaleFactory :: Word -> Factory a -> Factory a
scaleFactory s f = f { workerCount = s * workerCount f , inputs = scaleFactory s <$> inputs f }

instance HasThroughput a => HasThroughput (Factory a) where
  outputPerSecond f = scaleThroughput (fromIntegral (workerCount f)) <$> outputPerSecond (worker f)
  inputPerSecond f = collectThroughput $ do
    t <- inputPerSecond (worker f)
    case inputs f Map.!? item t of
      Just f' -> inputPerSecond f'
      Nothing -> return $ scaleThroughput (fromIntegral (workerCount f)) t

-- | Throughputs of all subfactories
internalThroughputPerSecond :: HasThroughput a => Factory a -> [ Throughput Word Second ]
internalThroughputPerSecond = collectThroughput . go
  where
  go f = do
    f' <- toList $ inputs f
    toList (outputPerSecond f') ++ go f'

{- |
  Finds the optimal ratio producing the item given a list of 'Recipe's.
-}
-- XXX find shared subtrees
optimalFactory :: Item -> [ Recipe ] -> Maybe (Factory Recipe)
optimalFactory it context = do
  outputRecipe <- findProduct it context
  let optimizedInputs = do
         t <- inputPerSecond outputRecipe
         -- nonexistent solution should cause failure!
         f <- maybeToList $ optimalFactory (item t) context
         return (f, t)
  let inputAdjustment = foldl lcmRatio 1 $ do
        (f , ips) <- optimizedInputs
        ops <- toList $ outputPerSecond f
        guard (item ips == item ops)
        return $ lcmRatio (throughput ips) (throughput ops) / throughput ips

  return $ Factory
    { inputs = fromList $ do
        (f , ips) <- optimizedInputs
        ops <- toList $ outputPerSecond f
        guard $ item ips == item ops
        let f' = scaleFactory (ratioToIntegral $ throughput ips * inputAdjustment / throughput ops) f
        return $ (,) (item ops) f'

    , worker = outputRecipe
    , workerCount = ratioToIntegral inputAdjustment
    }

simplFactoryShow :: Factory Recipe -> String
simplFactoryShow f = unlines (aux f) where
  aux f' =
    [ intercalate "\n" $ (replicate 2 ' ' ++) <$> aux f'' | f'' <- toList (inputs f') ]
    ++
    [ unwords (toList (fmap (name . fst) (products (worker f')))) ++ " x " ++ show (workerCount f')]
