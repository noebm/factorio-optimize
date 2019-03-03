module Factory
where

import Prelude hiding (product)
import Data.Map (Map , fromList)
import Data.Foldable (toList)
import Control.Arrow (first, second)
import Data.Either -- (rights)

import Ratio
import Recipe

data Factory = Factory
  { inputs :: Map Item (Word , Factory) -- scaled factory
  , worker :: Recipe
  , workerCount :: Word -- output scale to make factory integral
  } deriving (Show)

factoryOutputPerSecond :: Factory -> Ratio Word
factoryOutputPerSecond f = productsPerSecond (worker f) * fromIntegral (workerCount f)

factoryProduct :: Factory -> Item
factoryProduct f = product (worker f)

scaleFactory :: Word -> Factory -> Factory
scaleFactory s f = f { workerCount = s * workerCount f , inputs = first (s *) <$> inputs f }

solveChain :: Item -> [ Recipe ] -> Maybe Factory
solveChain item context = do
  outputRecipe <- findProduct item context

  -- XXX find shared subtrees

  let factors
        = fmap (\ (ips , Right f) -> ((lcmRatio ips (factoryOutputPerSecond f), ips), f))
        . filter (isRight . snd)
        . fmap (second (\ item' -> maybe (Left item') Right $ solveChain item' context))
        $ ingredientsPerSecond outputRecipe
  let factor = foldl lcmRatio 1 $ (\((xps, ips) , _) -> xps / ips) <$> factors
  return $ Factory
    { inputs = fromList $ do
        ((_ , ips), f) <- factors
        let ops = factoryOutputPerSecond f
        let product = factoryProduct f
        return $ (,) product (ratioToIntegral (ips / ops * factor) , f)
    , worker = outputRecipe
    , workerCount = ratioToIntegral factor
    }

simplFactoryShow :: Factory -> String
simplFactoryShow f = unlines (aux f) where

  aux :: Factory -> [ String ]
  aux f =
    case toList (inputs f) of
      [] -> []
      xs -> "Subfactories" :
        [ unlines $ (replicate 2 ' ' ++) <$> aux (scaleFactory scale f) | (scale , f) <- xs ]
    ++
    [ name (product (worker f)) ++ " x " ++ show (workerCount f)]
