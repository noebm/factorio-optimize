{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Lua where

import HsLua

import Prelude hiding (product)
import Control.Monad
import Control.Applicative
import Data.Monoid
import Text.Printf
import Data.Foldable hiding (product)
import Data.Bifunctor
import Data.Bitraversable
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.List.NonEmpty as NonEmpty

import Data.Ratio

import Recipe
import Item

loadLibs :: LuaError e => [ FilePath ] -> LuaE e ()
loadLibs paths = for_ paths $ \path -> do
  status <- loadfile path
  liftIO $ printf "Loaded '%s': %s\n" path (show status)
  call 0 0

peekTable :: LuaError e => Peeker e a -> Peeker e [a]
peekTable entry = typeChecked "table" istable $ \idx -> cleanup $ do
  table <- liftLua $ absindex idx
  let collect idx = do
        t <- liftLua $ next table
        if t
          then do
            x <- entry idx `lastly` pop 1
            (x:) <$!> collect idx
          else return []
  liftLua pushnil
  collect top

peekIngredient :: LuaError e => Peeker e (Item, Word)
peekIngredient = choice [named, plain]
  where
  amount = peekIntegral
  plain = peekPair (fmap Item . peekString) amount
  -- TODO additional fields for type == fluids
  named idx = (,)
    <$> (Item <$> peekFieldRaw peekString "name" idx)
    <*> peekFieldRaw amount "amount" idx

peekIngredients :: LuaError e => Peeker e [(Item, Word)]
peekIngredients = peekFieldRaw (peekTable peekIngredient) "ingredients"

recipe :: LuaError e => Peeker e (String , (Recipe, Bool))
recipe idx = do
  name <- peekFieldRaw peekString "name" idx
  enabled <- peekFieldRaw peekBool "enabled" idx
  let aux idx = do
        ingredients <- peekIngredients idx
        results <- choice
          [ \idx -> do
              name <- peekFieldRaw peekString "result" idx
              count <- peekFieldRaw peekIntegral "result_count" idx <|> pure 1
              return [ (Item name , count) ]
          , peekFieldRaw (peekTable peekIngredient) "results"
          ]
          idx
        energy <- peekFieldRaw peekIntegral "energy_required" idx <|> pure 1
        return (ingredients, results, energy)

  -- some recipes have normal and expensive variants
  (ingredients, results, energy) <- aux idx <|> peekFieldRaw aux "normal" idx
  return $ (,) name $ (,) Recipe
    { ingredients = ingredients
    , products = NonEmpty.fromList results
    , energy = energy
    } enabled

peekRecipes :: LuaError e => Peek e (Map String (Recipe, Bool))
peekRecipes = do
  liftLua $ getglobal "data"
  let aux = peekFieldRaw (peekTable recipe) "recipe"
  peekFieldRaw (fmap Map.fromList . aux) "raw" top

loadLua :: IO (Map String (Recipe, Bool))
loadLua = run @HsLua.Exception $ do
  openlibs

  let libs =
        [ "./factorio-data/core/lualib/dataloader.lua"
        -- , "./factorio-data/core/lualib/util.lua"
        , "./factorio-data/base/prototypes/recipe.lua"
        ]
  loadLibs libs
  forcePeek peekRecipes
