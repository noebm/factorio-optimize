{-# LANGUAGE ScopedTypeVariables #-}
module Throughput where

import Data.Ratio
import Data.Proxy
import Data.List.NonEmpty (NonEmpty)
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

import Item

data Tick
data Second
data Minute
data Hour
data Day

class Time t where
  timeInSeconds :: Integral a => Proxy t -> Ratio a

instance Time Tick where
  timeInSeconds _ = 1 % 60

instance Time Second where
  timeInSeconds _ = 1

instance Time Minute where
  timeInSeconds _ = 60

instance Time Hour where
  timeInSeconds _ = 60 * 60

instance Time Day where
  timeInSeconds _ = 60 * 60 * 24

newtype Throughput a t = Throughput { throughput :: a }
  deriving Show

scaleThroughput :: Num a => a -> Throughput a t -> Throughput a t
scaleThroughput r t = t { throughput = r * throughput t }

convertThroughput :: forall t1 t2 a . (Time t1, Time t2, Integral a)
                  => Throughput (Ratio a) t1 -> Throughput (Ratio a) t2
convertThroughput t = t { throughput = k * throughput t }
  where k = timeInSeconds (Proxy :: Proxy t1) / timeInSeconds (Proxy :: Proxy t2)

class HasThroughput x where
  outputPerSecond :: x -> Map Item (Throughput (Ratio Word) Second)
  inputPerSecond :: x -> Map Item (Throughput (Ratio Word) Second)

findInputPerSecond :: HasThroughput x => x -> Item -> Maybe (Ratio Word)
findInputPerSecond b i = fmap throughput $ inputPerSecond b Map.!? i

findOutputPerSecond :: HasThroughput x => x -> Item -> Maybe (Ratio Word)
findOutputPerSecond b i = fmap throughput $ outputPerSecond b Map.!? i

throughputMap :: Num a => Map.Map Item (Throughput a t) -> Map.Map Item a
throughputMap = Map.fromListWith (+) . Map.toList . fmap throughput

toThroughputList :: Map.Map Item a -> Map Item (Throughput a t)
toThroughputList = fmap Throughput

collectThroughput :: Integral a => Map.Map Item (Throughput a t) -> Map.Map Item (Throughput a t)
collectThroughput = toThroughputList . throughputMap

