module Lib
--    ( someFunc
--    )
where

import Prelude hiding (product)
import Data.Foldable hiding (product)
import Data.Traversable
import Control.Arrow (first, second)
import Data.Either -- (rights)
import Data.Maybe (maybeToList)
import Data.Map (Map , fromList)

import Ratio

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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
Items
-}
-- ironOre = Item "Iron Ore"
-- copperOre = Item "Copper Ore"

ironPlate = Item "Iron Plate"
copperPlate = Item "Copper Plate"
gear = Item "Iron gear wheel"

yellowBelt = Item "Transport belt"
inserter = Item "Inserter"
greenCircuit = Item "Electronic Circuit"

copperCable = Item "Copper cable"

redScience = Item "Automation science pack"
greenScience = Item "Logistic science pack"

{-
Recipes
-}
gearRecipe :: Recipe
gearRecipe = Recipe
  { ingredients = [ (2 , ironPlate) ]
  , product = gear
  , productCount = 1
  , craftingTime = 0.5
  }

redScienceRecipe = Recipe
  { ingredients = [(1, copperPlate) , (1, gear)]
  , product = redScience
  , productCount = 1
  , craftingTime = 5
  }

copperCableRecipe = Recipe
  { ingredients = [(1 , copperPlate)]
  , product = copperCable
  , productCount = 2
  , craftingTime = 0.5
  }

greenCircuitRecipe = Recipe
  { ingredients = [ (3 , copperCable) , (1, ironPlate)]
  , product = greenCircuit
  , productCount = 1
  , craftingTime = 0.5
  }

inserterRecipe = Recipe
  { ingredients = [ (1, greenCircuit), (1, gear), (1, ironPlate) ]
  , product = inserter
  , productCount = 1
  , craftingTime = 0.5
  }

yellowBeltRecipe = Recipe
  { ingredients = [ (1, ironPlate) , (1 , gear)]
  , product = yellowBelt
  , productCount = 2
  , craftingTime = 0.5
  }

greenScienceRecipe = Recipe
  { ingredients = [ (1, inserter), (1, yellowBelt)]
  , product = greenScience
  , productCount = 1
  , craftingTime = 6
  }


{-
  find a building that produces the item
-}
findProduct :: Item -> [ Recipe ] -> Maybe Recipe
findProduct item = find (\ building -> product building == item)

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
  let factor = foldl lcmRatio 1 $ (\((xps, ips) , f) -> xps / ips) <$> factors
  return $ Factory
    { inputs = fromList $ do
        ((_ , ips), f) <- factors
        let ops = factoryOutputPerSecond f
        let product = factoryProduct f
        return $ (,) product $ (ratioToIntegral (ips / ops * factor) , f)
    , worker = outputRecipe
    , workerCount = ratioToIntegral factor
    }

simplFactoryShow :: Factory -> String
simplFactoryShow f = unlines (aux f) where

  aux :: Factory -> [ String ]
  aux f =
    case (toList (inputs f)) of
      [] -> []
      xs -> "Subfactories" :
        [ unlines $ (replicate 2 ' ' ++) <$> aux (scaleFactory scale f) | (scale , f) <- xs ]
    ++
    [ name (product (worker f)) ++ " x " ++ show (workerCount f)]
