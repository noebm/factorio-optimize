module Item where

newtype Item = Item
  { name :: String }
  deriving (Eq, Ord, Show)
