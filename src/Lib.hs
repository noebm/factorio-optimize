module Lib
--    ( someFunc
--    )
where

import qualified Prelude
import Prelude hiding (product)
import Data.Foldable hiding (product)
import Data.Traversable
import Control.Arrow (first, second)
import Data.Ratio
import Data.Either -- (rights)
import Data.Maybe (maybeToList)
import Data.Function (on)
import Data.Map (Map , fromList)

lcmRatio :: Integral a => Ratio a -> Ratio a -> Ratio a
lcmRatio x y = (lcm `on` numerator) x y % (gcd `on` denominator) x y

gcdRatio :: Integral a => Ratio a -> Ratio a -> Ratio a
gcdRatio x y = (gcd `on` numerator) x y % (lcm `on` denominator) x y

ratioToIntegral :: Integral a => Ratio a -> a
ratioToIntegral x = numerator x * denominator x

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Item = Item
  { name :: String }
  deriving (Eq, Ord, Show)

data Building = Building
  { ingredients :: [ (Word , Item) ]
  , product :: Item
  , productCount :: Word
  , craftingTime :: Ratio Word
  } deriving (Eq, Show)

ingredientsPerSecond :: Building -> [ (Ratio Word, Item) ]
ingredientsPerSecond b = first ((/ t) . fromIntegral) <$> ingredients b
  where t = craftingTime b

ingredientPerSecond :: Building -> Item -> Maybe (Ratio Word)
ingredientPerSecond b i = fmap fst $ find ((== i) . snd) $ ingredientsPerSecond b

ingredientsItems :: Building -> [ Item ]
ingredientsItems b = snd <$> ingredients b

productsPerSecond :: Building -> Ratio Word
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
Buildings
-}
gearBuilding :: Building
gearBuilding = Building
  { ingredients = [ (2 , ironPlate) ]
  , product = gear
  , productCount = 1
  , craftingTime = 0.5
  }

redScienceBuilding = Building
  { ingredients = [(1, copperPlate) , (1, gear)]
  , product = redScience
  , productCount = 1
  , craftingTime = 5
  }

copperCableBuilding = Building
  { ingredients = [(1 , copperPlate)]
  , product = copperCable
  , productCount = 2
  , craftingTime = 0.5
  }

greenCircuitBuilding = Building
  { ingredients = [ (3 , copperCable) , (1, ironPlate)]
  , product = greenCircuit
  , productCount = 1
  , craftingTime = 0.5
  }

{-
  find a building that produces the item
-}
findProduct :: Item -> [ Building ] -> Maybe Building
findProduct item = find (\ building -> product building == item)

data Factory = Factory
  { inputs :: Map Item (Word , Factory) -- scaled factory
  , worker :: Building
  , workerCount :: Word -- output scale to make factory integral
  } deriving (Show)

factoryOutputPerSecond :: Factory -> Ratio Word
factoryOutputPerSecond f = productsPerSecond (worker f) * fromIntegral (workerCount f)

factoryProduct :: Factory -> Item
factoryProduct f = product (worker f)

-- scienceLaboratory :: 

-- factoryScale :: Ratio Word -> Factory -> Ratio Word
-- factoryScale requiredPerSecond f = factoryOutputPerSecond f / requiredPerSecond

-- undistributeLeft :: Either (a , b) (a , c) -> (a , Either b c)
-- undistributeLeft (Left (x , y)) = (x , Left y)
-- undistributeLeft (Right (x , z)) = (x , Right z)

solveChain :: Item -> [ Building ] -> Maybe Factory
solveChain item context = do
  outputBuilding <- findProduct item context

  -- XXX find shared subtrees

  let factors
        = fmap (\ (ips , Right f) -> ((lcmRatio ips (factoryOutputPerSecond f), ips), f))
        . filter (isRight . snd)
        . fmap (second (\ item' -> maybe (Left item') Right $ solveChain item' context))
        $ ingredientsPerSecond outputBuilding
  let factor = Prelude.product $ (\((xps, ips) , f) -> xps / ips) <$> factors
        -- foldl lcmRatio 1 $ fmap factoryOutputPerSecond subfactories

  -- Nothing
  return $ Factory
    { inputs = fromList $ do
        ((_ , ips), f) <- factors
        let ops = factoryOutputPerSecond f
        let product = factoryProduct f
        return $ (,) product $ (ratioToIntegral (ips / ops * factor) , f)
    , worker = outputBuilding
    , workerCount = ratioToIntegral factor
    }

simplFactoryShow :: Factory -> String
simplFactoryShow f = unlines (aux f) where

  aux :: Factory -> [ String ]
  aux f =
    case (toList (inputs f)) of
      [] -> []
      xs -> "Subfactories" :
        [ unlines $ (replicate 2 ' ' ++) <$> aux f | (scale , f) <- xs ]
            -- [ concat $ (\(scale , f) -> (show scale ++ " -") : unlines (aux f)) <$> xs ]
    ++
    [ "Building: " ++ name (product (worker f)) ++ " x " ++ show (workerCount f)]
