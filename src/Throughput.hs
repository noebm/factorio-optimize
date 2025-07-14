{-# LANGUAGE ScopedTypeVariables #-}

module Throughput where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Ratio
import Item

data Tick

data Second

data Minute

data Hour

data Day

class Time t where
  timeInSeconds :: (Integral a) => Proxy t -> Ratio a

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

newtype Throughput a t = Throughput {throughput :: a}
  deriving (Show)

scaleThroughput :: (Num a) => a -> Throughput a t -> Throughput a t
scaleThroughput r t = t {throughput = r * throughput t}

convertThroughput ::
  forall t1 t2 a.
  (Time t1, Time t2, Integral a) =>
  Throughput (Ratio a) t1 ->
  Throughput (Ratio a) t2
convertThroughput t = t {throughput = k * throughput t}
  where
    k = timeInSeconds (Proxy :: Proxy t1) / timeInSeconds (Proxy :: Proxy t2)

class HasThroughput x where
  outputPerSecond :: x -> Map Item (Throughput (Ratio Word) Second)
  inputPerSecond :: x -> Map Item (Throughput (Ratio Word) Second)
