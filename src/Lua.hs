{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lua where

import Control.Applicative
import Control.Monad
import Data.Foldable
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio
import HsLua
import Item
import Recipe

loadLibs :: (LuaError e) => [FilePath] -> LuaE e ()
loadLibs paths = for_ paths $ \path -> do
  status <- loadfile $ Just path
  call 0 0

peekTable :: (LuaError e) => Peeker e a -> Peeker e [a]
peekTable entry = typeChecked "table" istable $ \idx -> cleanup $ do
  table <- liftLua $ absindex idx
  let collect idx = do
        t <- liftLua $ next table
        if t
          then do
            x <- entry idx `lastly` pop 1
            (x :) <$!> collect idx
          else return []
  liftLua pushnil
  collect top

peekIngredient :: (LuaError e) => Peeker e (Item, Word)
peekIngredient = choice [named, plain]
  where
    amount = peekIntegral
    plain = peekPair (fmap Item . peekString) amount
    -- TODO additional fields for type == fluids
    named idx =
      (,)
        <$> (Item <$> peekFieldRaw peekString "name" idx)
        <*> peekFieldRaw amount "amount" idx

peekIngredients :: (LuaError e) => Peeker e [(Item, Word)]
peekIngredients = peekFieldRaw (peekTable peekIngredient) "ingredients"

recipe :: (LuaError e) => Peeker e (String, (Recipe, Bool))
recipe idx = do
  name <- peekFieldRaw peekString "name" idx
  enabled <- peekFieldRaw peekBool "enabled" idx
  let aux idx = do
        ingredients <- peekIngredients idx
        results <-
          choice
            [ \idx -> do
                name <- peekFieldRaw peekString "result" idx
                count <- peekFieldRaw peekIntegral "result_count" idx <|> pure 1
                return [(Item name, count)],
              peekFieldRaw (peekTable peekIngredient) "results"
            ]
            idx
        energy <- (`approxRational` 0.01) <$> peekFieldRaw peekRealFloat "energy_required" idx <|> pure 0.5
        return $ Recipe ingredients (NonEmpty.fromList results) (fromRational energy)

  -- some recipes have normal and expensive variants
  recipe <- aux idx <|> peekFieldRaw aux "normal" idx
  return (name, (recipe, enabled))

peekRecipeList :: (LuaError e) => Peek e [(String, (Recipe, Bool))]
peekRecipeList = do
  liftLua $ getglobal "data"
  let aux = peekFieldRaw (peekTable recipe) "recipe"
  peekFieldRaw aux "raw" top

luaRecipeList :: IO [(String, (Recipe, Bool))]
luaRecipeList = run @HsLua.Exception $ do
  openlibs

  let libs =
        [ "./factorio-data/core/lualib/dataloader.lua",
          -- , "./factorio-data/core/lualib/util.lua"
          "./factorio-data/base/prototypes/recipe.lua"
        ]
  loadLibs libs
  forcePeek peekRecipeList
