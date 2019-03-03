module Factory
where

import Prelude hiding (product)
import Data.Foldable (toList, find)
import Control.Arrow (first, second)
import Data.Either -- (rights)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Data.Map (Map , fromList)
import qualified Data.Map as Map

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

requiredInputs :: Factory -> [ (Item, Ratio Word) ]
requiredInputs f = Map.toList $ Map.unionsWith (+) $ fmap (fromList . return) $ do
  (item , itemsPerSecond) <- ingredientsPerSecond (worker f)
  case inputs f Map.!? item of
    Just (f' , mult) -> second (fromIntegral mult *) <$> requiredInputs f'
    Nothing -> return (item, itemsPerSecond * fromIntegral (workerCount f))

solveChain :: Item -> [ Recipe ] -> Maybe Factory
solveChain item context = do
  outputRecipe <- findProduct item context

  -- XXX find shared subtrees

  let factors
        = fmap (\ (Right f, ips) -> ((lcmRatio ips (factoryOutputPerSecond f), ips), f))
        . filter (isRight . fst)
        . fmap (first (\ item' -> maybe (Left item') Right $ solveChain item' context))
        $ ingredientsPerSecond outputRecipe
  let factor = foldl lcmRatio 1 $ (\((xps, ips) , _) -> xps / ips) <$> factors
  return $ Factory
    { inputs = fromList $ do
        ((_ , ips), f) <- factors
        let ops = factoryOutputPerSecond f
        let product = factoryProduct f
        return $ (,) product (f , ratioToIntegral (ips / ops * factor))
    , worker = outputRecipe
    , workerCount = ratioToIntegral factor
    }

simplFactoryShow :: Factory -> String
simplFactoryShow f = unlines (aux f) where
  aux :: Factory -> [ String ]
  aux f =
    [ intercalate "\n" $ (replicate 2 ' ' ++) <$> aux (scaleFactory scale f)
    | (f , scale) <- toList (inputs f) ]
    ++
    [ name (product (worker f)) ++ " x " ++ show (workerCount f)]
