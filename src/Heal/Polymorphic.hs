{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Heal.Polymorphic
 ( pointMutation
 , pointMutationStd
 , pointMutationLoci
 , pointMutationLociStd
 , repopulate
 , onePointCrossover 
 , crossPair1Point
 , cross
 , crossAligned
 , crossover1pAlign
 , wilt 
 , rouletteWheel 
 , tournamentSelection 
 --, susPoly 
 , standardGeneticAlgorithm 
 ) where


import System.Random.MWC.Monad as MWC
import System.Random.MWC
import System.Random.MWC.CondensedTable.Monad as MWC
import System.Random.MWC.Distributions.Monad as MWC

import Data.Traversable as T
import qualified Data.Foldable as F
import Data.Align
import Data.These
import Data.Witherable
import qualified Data.Vector as V
import Data.Functor.Compose

import Control.Monad.Primitive.Class
import Control.Monad
import Control.Arrow
import Control.Applicative

import Heal.Utilities
--import qualified Heal.Monomorphic as Mono


{- GA implemented polymorphic over the population, individual, and locus types -}

-- generalize to multipoint crossover
-- break out pure from random parts when appropriate
-- consider using representable for much of this

{- Mutation -}
-- Point mutation on an individual. Uses standard probability distribution on
-- the locus type (requires an instance of Variant)
pointMutationLociStd ::
  (Traversable f, MonadPrim m, Variate a) =>
  Double -> f a -> Rand m (f a)
pointMutationLociStd prob p = pointMutationLoci prob (const MWC.uniform) p

-- Point mutation on an individual. Requires a function to mutate a single locus
pointMutationLoci ::
  (Traversable f, MonadPrim m) =>
  Double -> (a -> Rand m a) -> f a -> Rand m (f a)
pointMutationLoci prob f p = traverse (sometimesR prob f) p

-- Point mutation on a population. Uses standard probability distribution over
-- the type of the locus (requiring an instance of Variate)
pointMutationStd ::
  (Traversable f, Traversable g, MonadPrim m, Variate a) => 
  Double -> f (g a) -> Rand m (f (g a))
pointMutationStd prob p = pointMutation prob (const MWC.uniform) p

-- Point mutation on a population, given a way to mutate a locus
pointMutation ::
  (Traversable f, Traversable g, MonadPrim m) => 
  Double -> (a -> Rand m a) -> f (g a) -> Rand m (f (g a))
pointMutation prob f p =
  traverse (pointMutationLoci prob f) p

{- Crossover -}
onePointCrossover ::
  (MonadPrim m, Traversable f, Traversable g) =>
  Double -> f (g a) -> Rand m (f (g a))
onePointCrossover pc p =
  onPairs (sometimesR pc crossPair1Point) p

crossPair1Point ::
  (MonadPrim m, Traversable f) => (f a, f a) -> Rand m (f a, f a)
crossPair1Point (f, f') = 
  let fList  = F.toList f
      fList' = F.toList f'
      len    = max (length fList) (length fList')
  in do crossPoint <-  MWC.uniformR (0, len)
        let (crossed, crossed') = cross crossPoint fList fList'
        return (repopulate f crossed, repopulate f' crossed')

-- Operate on adjacent pairs of items within a foldable, by applying a 
-- function that returns a traverable container.
onPairs :: 
  (Foldable t, Traversable t, Applicative f) =>
  ((a, a) -> f (a, a)) -> t a -> f (t a)
onPairs f t = 
  -- pair up elements using a fold
  let pairs = snd $ F.foldl' pairup' (Nothing, []) t

      -- pair up adjacent elements by walking down the structure
      pairup' (maybeElem, ps) a = 
        case maybeElem of
          
          -- Starting new pair
          Nothing ->
            (Just a, ps)

          -- Complete a pair
          Just a' ->
            (Nothing, (a, a') : ps)

      -- replace elements within a structure with the new pairs
      unpair' (straggler, ps) _ =
        case straggler of
          Just a ->
            ((Nothing, ps), a)

          Nothing ->
            case ps of
              [] ->
                error "unpair' ran out of pairs!"

              ((a, a') : ps') ->
                ((Just a', ps'), a)

      -- unpair with a mapAccumL
      reimpair t ps = snd $ T.mapAccumL unpair' (Nothing, ps) t

  -- map function over pairs, and then fill original container back in with the results
   in reimpair t <$> T.traverse f pairs

unpair p = uncurry (++) $ unzip p

-- Fill in a traversable container given a list of elements.
-- Assumes that the list contains exactly the correct number of elements for each location
-- within the container.
repopulate :: (Traversable f) => f a -> [b] -> f b
repopulate fa as = snd $ T.mapAccumL populate as fa
  where populate []     a  = error "repopulate called without enough elements to fill the structure"
        populate (a:as) a' = (as, a)


unalign' al = go al ([], [])
  where go [] (firstList, secondList) = (reverse firstList, reverse secondList)
        go (This  a  :rest) (this, that) = go rest (a:this, that)
        go (That  b  :rest) (this, that) = go rest (this,   b:that)
        go (These a b:rest) (this, that) = go rest (a:this, b:that)

cross point as as' = unalign' $ snd $ mapAccumL crossPair point $ align as as'
  where crossPair 0 pair   = (0,      pair)
        crossPair n (These a b) = (n - 1, These b a)
        crossPair n elem = (0, elem)

-- One point crossover for a population that supports a zipping operation
-- provided by Align.
crossover1pAlign ::
  (Witherable f, Foldable f, MonadPrim m, Align f, Unalign f) =>
  (f a, f a) -> Rand m (f a, f a)
crossover1pAlign (f, f') =
  --  crossover point determined by longer individual
  let len = max (length f) (length f')
  -- determine crossover point randomly within individual
  in do point <-  MWC.uniformR (0, len)
  -- perform crossover of the individual as a pure function
        return $ crossAligned point f f'

-- One point crossover as pure function on two containers.
crossAligned :: (Witherable f, Align f, Unalign f) => Int -> f a -> f a -> (f a, f a)
crossAligned point f f' =
  -- map over the zipped containers, exchaning components up to the crossover point
  -- the final (wilt *** wilt) removes the inner Maybe wrappers from the pair of containers
  (wilt *** wilt) . unalign . snd . mapAccumL cross point $ aligned
  --    cross two individuals by swapping up to the crossover point.
  --    After that point, keep the individual's elements as they are
  where cross 0 a           = (0,   a)
        cross n (These a b) = (n-1, These b a)
        cross n a           = (n-1, a)
        -- zip the two containers
        aligned = align f f'

-- utility function to remove slots with Nothing in a Witherable container
wilt :: (Witherable f, Traversable f) => f (Maybe a) -> f a
wilt f = mapMaybe id f

{- Selection -}
-- TODO benchmark condensed probability tables vs more direct sampling

extendFitness (a, d) = ((a, d), d)

-- utility to transform a foldable into a vector
vect :: (Foldable f) => f a -> V.Vector a
vect = V.fromList . F.toList

-- Roulette Wheel Selection
rouletteWheel ::
  (MonadPrim m, Traversable f) =>
  f (a, Double) -> Rand m (f a)
rouletteWheel f = 
  let table = MWC.tableFromWeights $ vect f
   in traverse (const (genFromTable table)) f

-- Tournament Selection
tournamentSelection ::
  (Traversable f, Foldable f, MonadPrim m) =>
  Int -> f (a, Double) -> Rand m (f a)
tournamentSelection k f = 
  let as = fmap extendFitness $ vect f
      table = MWC.tableFromWeights as
      tourn = selectPlayers k table >>= playTournment
  --in (repopulate f . fmap fst) <$> replicateM (V.length as) tourn
  in traverse (fmap fst . const tourn) f

selectPlayers k table = V.replicateM k . genFromTable $ table

playTournment players = 
  do ix <- categorical $ fmap snd players
     return $ players V.! ix

-- Stochastic Universal Sampling
--susPoly ::
--  (Traversable f, MonadPrim m) => 
--  f (a, Double) -> Rand m (f a)
--susPoly f =
--  (repopulate f . F.toList) <$> (Mono.susMono (vect f))

{- Genetic Algorithm -}
standardGeneticAlgorithm ::
  (Variate a, Traversable f, Traversable g, MonadPrim m) =>
  Double ->                           -- point mutation probability
  Double ->                           -- crossover probability
  Int ->                              -- population size
  Int ->                              -- individual length
  Int ->                              -- generations
  (Int -> Int -> Rand m (f (g a))) -> -- create population
  (g a -> Double) ->                  -- evaluate an individual
  Rand m (f (g a))                    -- evolved population
standardGeneticAlgorithm pm pc ps is gens createPopulation eval =
  do population <- createPopulation ps is
     let evolve = pointMutationStd pm        >=>
                  onePointCrossover pc       >=>
                  (return . evaluation eval) >=>
                  tournamentSelection 2 -- rouletteWheel
     timesM gens evolve population

 
