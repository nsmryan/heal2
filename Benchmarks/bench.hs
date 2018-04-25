{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
import Criterion.Main

import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as UM

import Control.Monad

import System.Random.MWC.Monad as MWC
import System.Random.MWC
import System.Random.MWC.CondensedTable.Monad as MWC
import System.Random.MWC.Distributions.Monad as MWC

import Heal.Polymorphic as Poly
import Heal.Monomorphic as Mono
import Heal.Mutable as Mut


numIndividuals = 50
numLoci = 50

seqSeqBool :: S.Seq (S.Seq Bool)
seqSeqBool = S.replicate numIndividuals $ S.replicate numLoci True

vecVecBool :: V.Vector (V.Vector Bool)
vecVecBool = V.replicate numIndividuals $ V.replicate numLoci True

vecSeqBool = V.replicate numIndividuals $ S.replicate (ceiling (fromIntegral numLoci / 32.0)) 0

listListBool :: [[Bool]]
listListBool = L.replicate numIndividuals $ L.replicate numLoci True

evaledPopulations = runWithCreate $ do
  fits <- replicateM numIndividuals MWC.uniform
  return (S.zip seqSeqBool   (S.fromList fits),
          V.zip vecVecBool   (V.fromList fits),
          L.zip listListBool fits)

main :: IO ()
main = defaultMain
  [ bgroup "polymut" 
    [ bench "pm seqseq 0.01"   $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.01 seqSeqBool
    , bench "pm vecvec 0.01"   $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.01 vecVecBool
    , bench "pm listlist 0.01" $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.01 listListBool

    , bench "pm seqseq 0.8"   $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.8 seqSeqBool
    , bench "pm vecvec 0.8"   $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.8 vecVecBool
    , bench "pm listlist 0.8" $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.8 listListBool
    ]
  , bgroup "polymonomut" 
    [ bench "poly pm seqseq 0.01"   $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.01 seqSeqBool
    , bench "poly pm vecvec 0.01"   $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.01 vecVecBool
    , bench "poly pm listlist 0.01" $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.01 listListBool
    , bench "mono pm 0.01" $ nfIO $ runWithCreate $ pointMutationBit 0.01 numIndividuals numLoci vecSeqBool

    , bench "poly pm seqseq 0.8"   $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.8 seqSeqBool
    , bench "poly pm vecvec 0.8"   $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.8 vecVecBool
    , bench "poly pm listlist 0.8" $ nfIO $ runWithCreate $ Poly.pointMutationStd 0.8 listListBool
    , bench "mono pm 0.8" $ nfIO $ runWithCreate $ pointMutationBit 0.8 numIndividuals numLoci vecSeqBool
    ]
  , bgroup "monomut" 
    [ bench "mono pm 0.01" $ nfIO $ runWithCreate $ pointMutationBit 0.01 numIndividuals numLoci vecSeqBool
    , bench "mono pm 0.20" $ nfIO $ runWithCreate $ pointMutationBit 0.20 numIndividuals numLoci vecSeqBool
    , bench "mono pm 0.40" $ nfIO $ runWithCreate $ pointMutationBit 0.40 numIndividuals numLoci vecSeqBool
    , bench "mono pm 0.60" $ nfIO $ runWithCreate $ pointMutationBit 0.60 numIndividuals numLoci vecSeqBool
    , bench "mono pm 0.80" $ nfIO $ runWithCreate $ pointMutationBit 0.80 numIndividuals numLoci vecSeqBool
    , bench "mono pm 1.00" $ nfIO $ runWithCreate $ pointMutationBit 1.00 numIndividuals numLoci vecSeqBool
    ]
  , bgroup "gaind" 
    [ bench "polysmall" $ nfIO $ runWithCreate $
      Poly.standardGeneticAlgorithm 0.01
                                    0.6
                                    10  -- is
                                    10  -- ps
                                    100 -- gens
                                    (\ is ps -> V.replicateM ps $ S.replicateM is MWC.uniform)
                                    F.sum

    , bench "monosmall" $ nfIO $ runWithCreate $
      Mono.standardGeneticAlgorithm 0.01
                                    0.6
                                    10  -- is
                                    10  -- ps
                                    100 -- gens
                                    monomorphicPopulation
                                    (fromIntegral . F.sum . fmap popCount)

    , bench "naivesmall" $ nfIO $ runWithCreate $
      Mut.standardGeneticAlgorithm 0.01
                                   0.6
                                   10
                                   10
                                   100
                                   iovectorPopcount

    , bench "polymed" $ nfIO $ runWithCreate $
      Poly.standardGeneticAlgorithm 0.01
                                    0.6
                                    100
                                    10
                                    100
                                    (\ is ps -> V.replicateM ps $ S.replicateM is MWC.uniform)
                                    F.sum

    , bench "monomed" $ nfIO $ runWithCreate $
      Mono.standardGeneticAlgorithm 0.01
                                    0.6
                                    100
                                    10
                                    100
                                    monomorphicPopulation
                                    (fromIntegral . F.sum . fmap popCount)

    , bench "naivemed" $ nfIO $ runWithCreate $
      Mut.standardGeneticAlgorithm 0.01
                                   0.6
                                   100
                                   10
                                   100
                                   iovectorPopcount

    , bench "polylarge" $ nfIO $ runWithCreate $
      Poly.standardGeneticAlgorithm 0.01
                                    0.6
                                    1000
                                    10
                                    100
                                    (\ is ps -> V.replicateM ps $ S.replicateM is MWC.uniform)
                                    F.sum

    , bench "monolarge" $ nfIO $ runWithCreate $
      Mono.standardGeneticAlgorithm 0.01
                                    0.6
                                    1000
                                    10
                                    100
                                    monomorphicPopulation
                                    (fromIntegral . F.sum . fmap popCount)

    , bench "naivelarge" $ nfIO $ runWithCreate $
      Mut.standardGeneticAlgorithm 0.01
                                   0.6
                                   1000
                                   10
                                   100
                                   iovectorPopcount
    ]
  , bgroup "gagens" 
    [ bench "polysmall" $ nfIO $ runWithCreate $
      Poly.standardGeneticAlgorithm 0.01
                                    0.6
                                    1000  -- is
                                    10  -- ps
                                    10 -- gens
                                    (\ is ps -> V.replicateM ps $ S.replicateM is MWC.uniform)
                                    F.sum

    , bench "monosmall" $ nfIO $ runWithCreate $
      Mono.standardGeneticAlgorithm 0.01
                                    0.6
                                    1000  -- is
                                    10  -- ps
                                    10 -- gens
                                    monomorphicPopulation
                                    (fromIntegral . F.sum . fmap popCount)

    , bench "naivesmall" $ nfIO $ runWithCreate $
      Mut.standardGeneticAlgorithm 0.01
                                   0.6
                                   1000
                                   10
                                   10
                                   iovectorPopcount

    , bench "polymed" $ nfIO $ runWithCreate $
      Poly.standardGeneticAlgorithm 0.01
                                    0.6
                                    1000
                                    10
                                    100
                                    (\ is ps -> V.replicateM ps $ S.replicateM is MWC.uniform)
                                    F.sum

    , bench "monomed" $ nfIO $ runWithCreate $
      Mono.standardGeneticAlgorithm 0.01
                                    0.6
                                    1000
                                    10
                                    100
                                    monomorphicPopulation
                                    (fromIntegral . F.sum . fmap popCount)

    , bench "naivemed" $ nfIO $ runWithCreate $
      Mut.standardGeneticAlgorithm 0.01
                                   0.6
                                   1000
                                   10
                                   100
                                   iovectorPopcount

    , bench "polylarge" $ nfIO $ runWithCreate $
      Poly.standardGeneticAlgorithm 0.01
                                    0.6
                                    1000
                                    10
                                    1000
                                    (\ is ps -> V.replicateM ps $ S.replicateM is MWC.uniform)
                                    F.sum

    , bench "monolarge" $ nfIO $ runWithCreate $
      Mono.standardGeneticAlgorithm 0.01
                                    0.6
                                    1000
                                    10
                                    1000
                                    monomorphicPopulation
                                    (fromIntegral . F.sum . fmap popCount)

    , bench "naivelarge" $ nfIO $ runWithCreate $
      Mut.standardGeneticAlgorithm 0.01
                                   0.6
                                   1000
                                   10
                                   1000
                                   iovectorPopcount
    ]
  , env evaledPopulations $ \ ~(s,v,l) -> bgroup "polyselect" 
    [ bench "SUS seq"        $ nfIO $ runWithCreate $ Poly.rouletteWheel s
    , bench "roulette seq"   $ nfIO $ runWithCreate $ tournamentSelection 2 s
    , bench "tournament seq" $ nfIO $ runWithCreate $ susPoly s

    , bench "SUS vec"        $ nfIO $ runWithCreate $ Poly.rouletteWheel v
    , bench "roulette vec"   $ nfIO $ runWithCreate $ tournamentSelection 2 v
    , bench "tournament vec" $ nfIO $ runWithCreate $ susPoly v

    , bench "SUS list"        $ nfIO $ runWithCreate $ Poly.rouletteWheel l
    , bench "roulette list"   $ nfIO $ runWithCreate $ tournamentSelection 2 l
    , bench "tournament list" $ nfIO $ runWithCreate $ susPoly l
    ]
  ]


