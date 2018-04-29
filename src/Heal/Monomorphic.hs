{-# LANGUAGE BangPatterns #-}
module Heal.Monomorphic
( pointMutationBit
, packedBitsScope
, lociScope
, monomorphicPopulation
, monoBitPopulation
, monoBytePopulation
, genesNeeded 
--, splitIx 
, accumIx
, ixToMovements 
, crossoverScoped
, cross
, cross'
, cross''
, repairCrossed
, susMono 
, comb
, teeth
, toothWidth
, standardGeneticAlgorithm 
, bitIndexes
) where

import Prelude as P
import Debug.Trace
import System.Random.MWC.Monad as MWC
import System.Random.MWC.CondensedTable.Monad as MWC
import Control.Monad.Primitive.Class
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Word
import Data.Bits
import Data.List as L
import Data.Foldable as F
import Heal.Scope
import Heal.Utilities


{- GA implemented for a particular, efficient data structure -}
type BitPop = V.Vector (S.Seq Word32)

onIxesM scope ixsM f s = 
  do ixs <- ixsM
     updated <- foldM (\t n -> modifyM scope f $ move scope n t) (into scope s) ixs
     return $ outOf scope updated

onIxes scope ixs f pSize iSize s = 
  outOf scope $ foldl' (\t n -> modify scope f $ move scope n t) (into scope s) ixs

{- Point Mutation -}
pointMutationBit ::
  (MonadPrim m) =>
  Double -> -- probability of mutation
  Int ->    -- individual size (number of loci)
  Int ->    -- population size
  BitPop -> -- initial population
  Rand m BitPop
pointMutationBit pm pSize iSize s = 
  onIxesM packedBitsScope (bitIndexes pSize iSize pm) (const MWC.uniform) s

bitIndexes pSize iSize pm =
  do ixs <- generateIndices pm (pSize * iSize)
     return $ ixToMovements iSize ixs

-- TODO this could be generalized to work in layers
ixToMovements !indLen !ixs = snd $ mapAccumL (accumIx indLen) 0 ixs

-- go forward by an offset within the population
-- this checks whether we have left the current loci, 
-- and then whether we have left the current individual
accumIx :: Int -> Int -> Int -> (Int, (Maybe Int, (Maybe Int, Int)))
accumIx !indLen !acc !offset =
  let acc' = acc + offset
      -- index within population
      pIx  = acc  `div` indLen
      pIx' = acc' `div` indLen
      -- index within individual
      iIx  = (acc  `mod` indLen) `div` 32
      iIx' = (acc' `mod` indLen) `div` 32
      -- index within loci
      lIx  = acc  `mod` 32
      lIx' = acc' `mod` 32
                 -- check if the location has left current individual
      movement = if pIx /= pIx'
                 -- moved to next individual- move by difference in ind index
                 then (Just (pIx' - pIx), (Just iIx', lIx'))
                 -- within same individual
                 else
                   -- check if the location has left current loci
                   if iIx /= iIx'
                   -- next word
                   then (Nothing, (Just (iIx' - iIx), lIx'))
                   -- within same word
                   else (Nothing, (Nothing, lIx' - lIx))
  in (acc', movement)

lociScope = 
  composeScopes vecIndexer seqIndexer

packedBitsScope =
  composeScopes vecIndexer $
    composeScopes seqIndexer (bitsIndexer :: Scope Word32 (Word32, Int) Int Bool)

{- Crossover -}
crossoverBitsScoped :: 
  (MonadPrim m) =>
  Double ->
  Int ->
  Int ->
  BitPop ->
  Rand m BitPop
crossoverBitsScoped pc pSize iSize p =
  do crossIndIxes <- generateIndices pc ((pSize `div` 2))
     onIxesM vecPair (return crossIndIxes) (crossBits iSize) p

crossBits ::
  (MonadPrim m) =>
  Int -> (S.Seq Word32, S.Seq Word32) -> Rand m (S.Seq Word32, S.Seq Word32)
crossBits indSize (as, as') = 
  do splitPoint <- MWC.uniformR (0, indSize - 1)
     let (first,  rest)  = S.splitAt lociIx as
         (first', rest') = S.splitAt lociIx as'
         lociIx = splitPoint `div` 32
         bitIx  = splitPoint `mod` 32
         a  = S.index rest  0
         a' = S.index rest' 0
         lowMask  = (bit bitIx) - 1
         highMask = complement lowMask
         aCross  = (a .&. highMask) .|. (a' .&. lowMask)
         aCross' = (a .&. lowMask)  .|. (a' .&. highMask)
     return (first S.>< (aCross' S.<| rest'), first' S.>< (aCross S.<| rest))

crossoverScoped :: 
  (MonadPrim m, Show a) =>
  Int ->
  Double ->
  Int ->
  Int ->
  V.Vector (S.Seq a) ->
  Rand m (V.Vector (S.Seq a))
crossoverScoped numPoints pc pSize iSize p =
  onIxesM vecPair (generateIndices pc ((pSize `div` 2))) (cross iSize numPoints) p

cross ::
  (MonadPrim m) =>
  Int -> Int -> (S.Seq a, S.Seq a) -> Rand m (S.Seq a, S.Seq a)
cross iSize numPoints indPair =
  do points <- L.sort <$> replicateM numPoints (MWC.uniformR (0, iSize))
     let offsets = snd $ mapAccumL (\acc n -> (n, n - acc)) 0 points
     return $ repairCrossed $ cross' offsets $ indPair

repairCrossed :: (Monoid a, Monoid t) => [(a, t)] -> (a, t)
repairCrossed pairs = (mconcat . map fst $ pairs, mconcat . map snd $ pairs)

cross' :: [Int] -> (S.Seq a, S.Seq a) -> [(S.Seq a, S.Seq a)]
cross' offsets (as, as') = 
  case offsets of
    [] ->
      [(as, as')]

    (offset : offsets') ->
      let (firsts, rests) = cross'' offset as as'
      in firsts : cross' offsets' rests

cross'' :: Int -> S.Seq a -> S.Seq a -> ((S.Seq a, S.Seq a), (S.Seq a, S.Seq a))
cross'' offset as as' =
  let (first,  rest)  = S.splitAt offset as
      (first', rest') = S.splitAt offset as'
   in ((first, first'), (rest', rest))

{- Selection -}

-- Roulette Wheel Selection
rouletteWheel ::
  (MonadPrim m) =>
  V.Vector (a, Double) -> Rand m (V.Vector a)
rouletteWheel p = 
  let table = MWC.tableFromWeights p
   in traverse (const (genFromTable table)) p

-- Stochastic Universal Sampling --
susMono ::
  (MonadPrim m) => 
  V.Vector (a, Double) -> Rand m (V.Vector a)
susMono v = do
  start <- MWC.uniformR (0.0, toothWidth $ V.map snd v)
  return $ comb start v

comb :: Double -> V.Vector (a, Double) -> V.Vector a
comb start p = teeth start p

toothWidth fits = V.sum fits / fromIntegral (V.length fits)

teeth :: Double -> V.Vector (a, Double) -> V.Vector a
teeth start p = 
  let width = toothWidth $ V.map snd p
      start' = if start == 0.0 then (width / 1000) else start
      walk acc (a, fit) =
        if acc > fit 
           then (acc - fit, V.empty)
           else
             let (n, r) = properFraction $ (fit - acc) / width
             in (width - r*width, V.replicate (n+1) a)
          in V.concat . snd . mapAccumL walk start' $ F.toList p

{- Genetic Algorithm -}
genesNeeded :: Int -> Int -> Int
genesNeeded geneLength numLoci = 
  let (d, m) = numLoci `divMod` geneLength
   in if m == 0
         then d
         else d + 1

monoBitPopulation ::
  (MonadPrim m) =>
  Int -> Int -> Rand m BitPop
monoBitPopulation pSize iSize = 
  V.replicateM pSize $ S.replicateM (genesNeeded 32 iSize) $ MWC.uniform

monomorphicPopulation ::
  (MonadPrim m) =>
  Int -> Int -> Rand m BitPop
monomorphicPopulation pSize iSize = 
  V.replicateM pSize $ S.replicateM iSize $ MWC.uniform

monoBytePopulation ::
  (MonadPrim m) =>
  Int -> Int -> Rand m (V.Vector (S.Seq Word8))
monoBytePopulation pSize iSize = 
  V.replicateM pSize $ S.replicateM iSize $ MWC.uniform

standardGeneticAlgorithm ::
  (MonadPrim m, MonadIO m) =>
  Double ->                         -- point mutation probability
  Double ->                         -- crossover probability
  Int ->                            -- population size
  Int ->                            -- individual length
  Int ->                            -- generations
  (Int -> Int -> Rand m BitPop) ->  -- create population
  (S.Seq Word32 -> Double) ->       -- evaluate an individual
  Rand m BitPop                     -- evolved population
standardGeneticAlgorithm pm pc ps is gens createPopulation eval =
  do population <- createPopulation ps is
     liftIO $ print population
     liftIO $ print "size:"
     liftIO $ print $ V.length population
     liftIO $ print "\n"
     liftIO $ print "ind size:"
     liftIO $ print $ is
     liftIO $ print "\n"
     let evolve = pointMutationBit pm ps is  >=>
                  crossoverBitsScoped pc ps is >=>
                  (return . evaluation eval) >=>
                  rouletteWheel
     timesM gens evolve population

