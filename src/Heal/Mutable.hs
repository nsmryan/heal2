{-# LANGUAGE FlexibleContexts #-}
module Heal.Mutable 
  ( pointMutationLoci
  , pointMutationLociStd
  , pointMutation
  , pointMutationStd
  , crossover1p
  , standardGeneticAlgorithm 
  , rouletteWheel
  , printIOVectorPop 
  , standardGeneticAlgorithmBits 
  , createVectorPopulation
  , createVectorPopulationBits 
  ) where

import Data.Vector as V
import Data.Vector.Unboxed.Mutable as UM
import System.Random.MWC 
import System.Random.MWC.Monad as MWC
import System.Random.MWC.Distributions.Monad as MWC
import System.Random.MWC.CondensedTable.Monad as MWC
import Control.Monad.Primitive.Class
import Control.Monad.Primitive
import Control.Monad as CM
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Word
import Data.Bits
import Text.Printf
import Heal.Utilities


{- GA implemented with mutable data structure -}

printIOVectorPop :: V.Vector (UM.IOVector Word32) -> IO ()
printIOVectorPop v =
  V.mapM_ (\ind -> eachIndex_ printWord ind >> putStrLn "") v
    where printWord :: Int -> Word32 -> IO ()
          printWord _ w = printf "%08X " w 

{- Mutation -}
pointMutationLociStd ::
  (Variate a, Unbox a) =>
  Double -> UM.IOVector a -> Rand IO (UM.IOVector a)
pointMutationLociStd prob p = pointMutationLoci prob (const MWC.uniform) p

pointMutationLoci ::
  (Unbox a) =>
  Double -> (a -> Rand IO a) -> UM.IOVector a -> Rand IO (UM.IOVector a)
pointMutationLoci prob f p = do
  onIndices [0 .. (UM.length p) - 1] (sometimesR prob f) p

onIndices ::
  (Unbox a, MonadIO m) =>
  [Int] -> (a -> m a) -> IOVector a -> m (IOVector a)
onIndices []       _ p = return p
onIndices (ix:ixs) f p = do
  a  <- liftIO $ UM.unsafeRead p ix
  a' <- f a
  liftIO $ UM.unsafeWrite p ix a'
  onIndices ixs f p

eachIndex ::
  (Unbox a, MonadIO m) =>
  (Int -> a -> m a) -> IOVector a -> m (IOVector a)
eachIndex f v = CM.mapM indexF [0 .. UM.length v - 1] >> return v
    where indexF index = do a  <- liftIO $ UM.unsafeRead v index
                            a' <- f index a
                            liftIO $ UM.unsafeWrite v index a'

eachIndex_ ::
  (Unbox a, MonadIO m) =>
  (Int -> a -> m ()) -> IOVector a -> m ()
eachIndex_ f v = CM.mapM_ indexF [0 .. UM.length v - 1]
    where indexF index = do a  <- liftIO $ UM.unsafeRead v index
                            f index a

pointMutationStd ::
  (Variate a, Unbox a) => 
  Double -> V.Vector (IOVector a) -> Rand IO (V.Vector (IOVector a))
pointMutationStd prob p = pointMutation (const MWC.uniform) prob p

pointMutation ::
  (Unbox a) => 
  (a -> Rand IO a) -> Double -> V.Vector (IOVector a) -> Rand IO (V.Vector (IOVector a))
pointMutation f prob p =
  V.mapM (pointMutationLoci prob f) p

pointMutationBits ::
  Int -> Double -> V.Vector (IOVector Word32) -> Rand IO (V.Vector (IOVector Word32))
pointMutationBits is prob p =
  V.mapM (eachIndex mutateWord) p
    where mutateWord lociIndex w =
            let numBits = if lociIndex == ((is `div` 32) -1) && (is `mod` 32 /= 0)
                          then is `mod` 32
                          else 32
             in CM.foldM mutateBit w [0 .. numBits - 1]
          mutateBit w bitIndex = sometimes prob (xor (bit bitIndex)) w
   
{- Crossover -}
crossover1p ::
  (Unbox a) =>
  Double -> V.Vector (IOVector a) -> Rand IO (V.Vector (IOVector a))
crossover1p pc p =
  let numPairs = V.length p `div` 2 in do
    CM.forM [0 .. numPairs - 1] $ \ i -> do
      shouldCross <- MWC.bernoulli pc
      if shouldCross
         then do let ind  = p V.! (2 * i)
                 let ind' = p V.! (2 * i + 1)
                 crossPoint <- MWC.uniformR (0, UM.length ind - 1)
                 CM.forM [0 .. crossPoint] $ \ i' -> do
                   a  <- liftIO $ UM.read ind  i'
                   a' <- liftIO $ UM.read ind' i'
                   liftIO $ UM.write ind  i' a'
                   liftIO $ UM.write ind' i' a
         else return [()]
    return p

crossover1pBits ::
  Int -> Double -> V.Vector (IOVector Word32) -> Rand IO (V.Vector (IOVector Word32))
crossover1pBits is pc p =
  let numPairs = V.length p `div` 2 in do
    CM.forM [0 .. numPairs - 1] $ \ indIndex -> do
      shouldCross <- MWC.bernoulli pc
      if shouldCross
         then do let ind  = p V.! (2 * indIndex)
                 let ind' = p V.! (2 * indIndex + 1)
                 iovectorCross is ind ind'
         else return ()
    return p

iovectorCross :: 
  (Unbox a, Bits a, MonadIO m, MonadPrim m, Num a) =>
  Int -> IOVector a -> IOVector a -> Rand m ()
iovectorCross is ind ind' =
  do crossPoint <- MWC.uniformR (0, is - 1)
     CM.forM [0 .. crossPoint `div` 32] $ \ index -> do
       a  <- liftIO $ UM.read ind  index
       a' <- liftIO $ UM.read ind' index
       let bitIx = crossPoint `mod` 32
       let lowMask  = (bit bitIx) - 1
       let highMask = complement lowMask
       let (aCross, aCross') = if index == crossPoint
                               then ((a .&. highMask) .|. (a' .&. lowMask),
                                     (a .&. lowMask)  .|. (a' .&. highMask))
                               else (a, a')
       liftIO $ UM.write ind  index aCross'
       liftIO $ UM.write ind' index aCross
     return ()

{- Selection -}
-- Roulette Wheel
-- TODO this allocates a whole new population and copies the old one
-- This could reuse old individuals to save on allocation.
rouletteWheel ::
  (Unbox a) =>
  V.Vector (IOVector a, Double) -> Rand IO (V.Vector (IOVector a))
rouletteWheel population = 
  let table = MWC.tableFromWeights population
      copyInd newPopulation index oldInd =
        do let newInd = newPopulation V.! index
           UM.copy newInd oldInd
           
   in do chosen <- traverse (const (genFromTable table)) population
         let ps = V.length population
         let is = UM.length $ fst $ population V.! 0
         population' <- V.replicateM ps $ UM.unsafeNew is
         V.imapM (copyInd population') chosen
         return population'


{- Genetic Algorithm -}
createVectorPopulation ::
  (Variate a, Unbox a) => 
  Int -> Int -> Rand IO (V.Vector (IOVector a))
createVectorPopulation ps is =
  V.replicateM ps $ UM.replicateM is MWC.uniform

createVectorPopulationBits ::
  Int -> Int -> Rand IO (V.Vector (IOVector Word32))
createVectorPopulationBits ps is =
  do let numLoci = (is `div` 32) + (if is `mod` 32 /= 0 then 1 else 0)
     p <- createVectorPopulation ps numLoci
     let maskUpper numBits w = w .&. (bit numBits - 1)
     let maskLastLoci ind = do loci <- UM.read ind $ numLoci - 1
                               let extraBits = is `mod` 32
                               let bitMask = if extraBits /= 0 then extraBits else 32
                               UM.write ind (numLoci-1) $ maskUpper bitMask loci
     liftIO $ V.mapM maskLastLoci p
     return p
                

standardGeneticAlgorithm ::
  (Variate a, Unbox a) =>
  Double ->                           -- point mutation probability
  Double ->                           -- crossover probability
  Int ->                              -- population size
  Int ->                              -- individual length
  Int ->                              -- generations
  (IOVector a -> Rand IO Double) ->   -- evaluate an individual
  Rand IO (V.Vector (IOVector a))     -- evolved population
standardGeneticAlgorithm pm pc ps is gens eval =
  do population <- createVectorPopulation ps is
     let evolve = pointMutationStd pm   >=>
                  crossover1p pc        >=>
                  evaluationM eval      >=>
                  rouletteWheel
     timesM gens evolve population

standardGeneticAlgorithmBits ::
  Double ->                                -- point mutation probability
  Double ->                                -- crossover probability
  Int ->                                   -- population size
  Int ->                                   -- individual length
  Int ->                                   -- generations
  (IOVector Word32 -> Rand IO Double) ->     -- evaluate an individual
  Rand IO (V.Vector (IOVector Word32))     -- evolved population
standardGeneticAlgorithmBits pm pc ps is gens eval =
  do population <- createVectorPopulationBits ps is 
     let evolve = pointMutationBits is pm  >=>
                  crossover1pBits is pc    >=>
                  evaluationM eval         >=>
                  rouletteWheel
     timesM gens evolve population


