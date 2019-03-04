{-# LANGUAGE ScopedTypeVariables #-}
module Throughput where

import Data.Ratio
import Data.Proxy
import Data.List.NonEmpty (NonEmpty)

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

newtype Throughput a t = Throughput { throughput :: Ratio a }

convertThroughput :: forall t1 t2 a . (Time t1, Time t2, Integral a) => Throughput a t1 -> Throughput a t2
convertThroughput = Throughput . (scale *) . throughput
  where scale = timeInSeconds (Proxy :: Proxy t1) / timeInSeconds (Proxy :: Proxy t2)

class HasThroughput x where
  outputPerSecond :: x -> NonEmpty (Throughput (Ratio Word) Second)
  inputPerSecond :: x -> [ Throughput (Ratio Word) Second ]
