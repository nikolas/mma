module Util (
	conv,
) where

-- generalize an Integral
conv :: (Integral a, Num b) => a -> b
conv = fromInteger . toInteger
