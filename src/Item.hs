{-# LANGUAGE DeriveLift #-}
module Item where

import Language.Haskell.TH.Syntax (Lift)

newtype Item = Item { name :: String }
  deriving (Eq, Ord, Show, Lift)
