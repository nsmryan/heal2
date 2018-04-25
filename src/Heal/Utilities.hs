{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
module Heal.Utilities
  ( sometimes
  , sometimesR
  , evaluation
  , evaluationM
  , timesM
  , generateIndices
  , iovectorPopcount
  , iovectorBitCount 
  ) where

import System.Random.MWC.Distributions.Monad as MWC
import System.Random.MWC.Monad as MWC
import System.Random.MWC
import Control.Monad.Primitive.Class
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Class
import Data.Vector.Unboxed.Mutable as UM
import Data.Word
import Data.Bits


-- Provide a PrimMonad instance to use Rand IO in certain places
instance PrimMonad (Rand IO) where
  type PrimState (Rand IO) = PrimState IO
  primitive = lift . primitive


{- Generate Indices -}
 -- start one index before ind, due to geometric1
generateIndices :: 
  (MonadPrim m) => Double -> Int -> Rand m [Int]
generateIndices p n =
  if p == 0.0
  then return []
  else unfoldM genSample (-1)
         where genSample current =
                 do offset <- MWC.geometric1 p 
                    let current' = current + offset
                    return $ if current' >= (n-1)
                             then Nothing
                             else Just (current', offset)

{- Utilities -}
unfoldM :: 
  (Monad m) => (s -> m (Maybe (s, a))) -> s -> m [a]
unfoldM f s =
  do result <- f s
     case result of
       Nothing -> 
         return []

       Just (s', a) ->
         (a :) <$> (unfoldM f s')

sometimes :: (MonadPrim m) => Double -> (a -> a) -> a -> Rand m a
sometimes prob f a = sometimesR prob (return . f) a

sometimesR :: (MonadPrim m) => Double -> (a -> Rand m a) -> a -> Rand m a
sometimesR prob f a = do
  b <- MWC.bernoulli prob
  if b then f a else return a

{- Evaluation -}
evaluate :: (a -> Double) -> a -> (a, Double)
evaluate f a = (a, f a)

evaluation :: (Functor f) => (a -> Double) -> f a -> f (a, Double)
evaluation f p = fmap (evaluate f) p

evaluationM ::
  (Traversable t, Monad f) => (a -> f Double) -> t a -> f (t (a, Double))
evaluationM f p = traverse (\a -> (,) <$> return a <*> f a) p

timesM :: (Monad m) => Int -> (a -> m a) -> a -> m a
timesM 0 _ p = return p
timesM n f p = (f p) >>= (timesM (n-1) f)

{- Pretty Print Population -}

{- Fitness functions -}
iovectorPopcount :: UM.IOVector Bool -> Rand IO Double
iovectorPopcount v = 
  foldM addItem 0 [0 .. UM.length v - 1]
  where addItem n i =
          do value <- UM.read v i
             return $ n + if value then 1 else 0

iovectorBitCount :: UM.IOVector Word32 -> Rand IO Double
iovectorBitCount v = 
  foldM addItem 0 [0 .. UM.length v - 1]
  where addItem !n !i =
          do value <- UM.read v i
             return $ n + fromIntegral (popCount value)

{- Bit Manipulation -}
swapBitsAt bitIx a a' =
  let lowMask  = (bit bitIx) - 1
      highMask = complement lowMask
      aCross  = (a .&. highMask) .|. (a' .&. lowMask)
      aCross' = (a .&. lowMask)  .|. (a' .&. highMask)
   in (aCross, aCross')

