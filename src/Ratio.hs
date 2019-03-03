module Ratio
  ( module Data.Ratio
  , lcmRatio
  , gcdRatio
  , ratioToIntegral
  )
where

import Data.Ratio
import Data.Function (on)

lcmRatio :: Integral a => Ratio a -> Ratio a -> Ratio a
lcmRatio x y = (lcm `on` numerator) x y % (gcd `on` denominator) x y

gcdRatio :: Integral a => Ratio a -> Ratio a -> Ratio a
gcdRatio x y = (gcd `on` numerator) x y % (lcm `on` denominator) x y

ratioToIntegral :: Integral a => Ratio a -> a
ratioToIntegral x = numerator x * denominator x
