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
  { ingredients = [ (ironPlate , 2) ]
  , product = gear
  , productCount = 1
  , craftingTime = 0.5
  }

redScienceRecipe = Recipe
  { ingredients = [(copperPlate, 1) , (gear, 1)]
  , product = redScience
  , productCount = 1
  , craftingTime = 5
  }

copperCableRecipe = Recipe
  { ingredients = [(copperPlate, 1)]
  , product = copperCable
  , productCount = 2
  , craftingTime = 0.5
  }

greenCircuitRecipe = Recipe
  { ingredients = [ (copperCable, 3) , (ironPlate, 1)]
  , product = greenCircuit
  , productCount = 1
  , craftingTime = 0.5
  }

inserterRecipe = Recipe
  { ingredients = [ (greenCircuit, 1), (gear, 1), (ironPlate, 1) ]
  , product = inserter
  , productCount = 1
  , craftingTime = 0.5
  }

yellowBeltRecipe = Recipe
  { ingredients = [ (ironPlate, 1) , (gear, 1)]
  , product = yellowBelt
  , productCount = 2
  , craftingTime = 0.5
  }

greenScienceRecipe = Recipe
  { ingredients = [ (inserter, 1), (yellowBelt, 1)]
  , product = greenScience
  , productCount = 1
  , craftingTime = 6
  }
