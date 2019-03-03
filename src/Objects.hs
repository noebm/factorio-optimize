module Objects
where

import Prelude hiding (product)
import Recipe

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
