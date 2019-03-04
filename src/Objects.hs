module Objects
where

import Prelude hiding (product)
import Recipe

ironPlate = Item "Iron Plate"
copperPlate = Item "Copper Plate"
gear = Item "Iron gear wheel"

yellowBelt = Item "Transport belt"
redBelt = Item "Fast transport belt"
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

redBeltRecipe = Recipe
  { ingredients = [ (gear, 5), (yellowBelt, 1) ]
  , product = redBelt
  , productCount = 1
  , craftingTime = 0.5
  }

greenScienceRecipe = Recipe
  { ingredients = [ (inserter, 1), (yellowBelt, 1)]
  , product = greenScience
  , productCount = 1
  , craftingTime = 6
  }

{-
  power
-}
-- XXX fluid behaviour and craftingTime not correct!

water = Item "Water"

offshorePump = Recipe
  { ingredients = []
  , product = water
  , productCount = 1200
  , craftingTime = 1
  }

-- 165 degrees
boilerSteam = Item "Steam"

boiler = Recipe
  { ingredients = [ (water, 60) ]
  , product = boilerSteam
  , productCount = 60
  , craftingTime = 1
  }

electricEnergy = Item "Electric Energy"

steamEngine = Recipe
  { ingredients = [ (boilerSteam, 30) ]
  , product = electricEnergy
  , productCount = 900
  , craftingTime = 1
  }

