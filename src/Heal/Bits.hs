module Heal.Bits
( pointMutation
, crossover
, crossoverBits
, crossBits
) where

import Data.Bits
import Data.Traversable as T
import qualified Data.Vector as V

import Control.Monad.Primitive.Class

import System.Random.MWC.Monad as MWC
import System.Random.MWC
import System.Random.MWC.CondensedTable.Monad as MWC
import System.Random.MWC.Distributions.Monad as MWC

import Heal.Utilities


{- GA with individuals represented as a single integer -}

{- Point Mutation -}
pointMutation ::
  (MonadPrim m, Traversable f, Bits b) =>
  Int -> Double -> f b -> Rand m (f b)
pointMutation n p population =
  traverse (mutateBits n p) population

mutateBits ::
  (MonadPrim m, Bits b) =>
  Int -> Double -> b -> Rand m b
mutateBits n p bits = 
  do ixs <- generateIndices p n
     return $ bits `xor` foldl setBit zeroBits ixs
 
 {- Crossover -}
crossover ::
  (MonadPrim m, FiniteBits b, Num b) =>
  Double -> V.Vector b -> Rand m (V.Vector b)
crossover p population =
  unpairVector <$> (traverse crossoverBits $ pairVector population)

pairVector v = V.zip v . V.drop 1 $ v

unpairVector v = uncurry (V.++) . V.unzip $ v

crossoverBits ::
  (MonadPrim m, FiniteBits b, Num b) =>
  (b, b) -> Rand m (b, b)
crossoverBits (b, b') = 
  do ix <- MWC.uniformR (0 :: Int, finiteBitSize b)
     return $ crossBits ix b b'

crossBits ::
  (FiniteBits b, Num b) =>
  Int -> b -> b -> (b, b)
crossBits ix b b' =
  let mask = (setBit zeroBits ix) - 1
      opmask = complement mask
  in ((b .&. mask) .|. (b' .&. opmask), (b' .&. mask) .|. (b .&. opmask))

