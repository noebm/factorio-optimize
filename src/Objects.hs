module Objects where

import System.IO.Unsafe
import Data.Map

import Recipe
import Lua

recipes :: Map String Recipe
recipes = fst <$> unsafePerformIO loadLua
