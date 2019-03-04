{-# LANGUAGE ScopedTypeVariables #-}
module Throughput where

import Data.Ratio
import Data.Proxy
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Foldable
import qualified Data.Map as Map

import Item

data Second
data Minute
data Hour
data Day

class Time t where
  timeInSeconds :: Integral a => Proxy t -> Ratio a

instance Time Second where
  timeInSeconds _ = 1

instance Time Minute where
  timeInSeconds _ = 60

instance Time Hour where
  timeInSeconds _ = 60 * 60

instance Time Day where
  timeInSeconds _ = 60 * 60 * 24

data Throughput a t = Throughput { item :: Item , throughput :: Ratio a }

scaleThroughput :: Integral a => Ratio a -> Throughput a t -> Throughput a t
scaleThroughput r t = t { throughput = r * throughput t }

convertThroughput :: forall t1 t2 a . (Time t1, Time t2, Integral a) => Throughput a t1 -> Throughput a t2
convertThroughput t = t { throughput = k * throughput t }
  where k = timeInSeconds (Proxy :: Proxy t1) / timeInSeconds (Proxy :: Proxy t2)

class HasThroughput x where
  outputPerSecond :: x -> NonEmpty (Throughput Word Second)
  inputPerSecond :: x -> [ Throughput Word Second ]

findInputPerSecond :: HasThroughput x => x -> Item -> Maybe (Ratio Word)
findInputPerSecond b i = fmap throughput $ find ((== i) . item) $ inputPerSecond b

findOutputPerSecond :: HasThroughput x => x -> Item -> Maybe (Ratio Word)
findOutputPerSecond b i = fmap throughput $ find ((== i) . item) $ outputPerSecond b

outputPerSecond' :: HasThroughput x => x -> Throughput Word Second
outputPerSecond' = NE.head . outputPerSecond

collectThroughput :: Integral a => [ Throughput a t ] -> Map.Map Item (Ratio a)
collectThroughput = Map.fromListWith (+) . fmap aux
  where aux (Throughput i k) = (i , k)

toThroughputList :: Map.Map Item (Ratio a) -> [ Throughput a t ]
toThroughputList = fmap (uncurry Throughput) . Map.toList

