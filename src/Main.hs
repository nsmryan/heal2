module Main where

import System.Random.MWC.Monad as MWC
import Options.Applicative
import Control.Monad
import Data.Bits
import Data.Word
import Data.Function
import Data.Foldable as F
import Data.Sequence as S
import qualified Data.List as L
import Data.Vector as V
import Text.Printf
import Heal.Mutable as Mut
import Heal.Polymorphic as Poly
import Heal.Monomorphic as Mono
import Heal.Utilities
import Heal.Mutable



data GaConfig = GaConfig 
  { gaConfigPm   :: Double
  , gaConfigPc   :: Double
  , gaConfigIs   :: Int
  , gaConfigPs   :: Int
  , gaConfigGens :: Int
  , gaConfigPoly :: Bool
  , gaConfigMono :: Bool
  , gaConfigMut  :: Bool
  }
  deriving (Show, Eq)

cmdArgs :: Parser GaConfig
cmdArgs = GaConfig
  <$> option auto
      (  long "point-mutation"
      <> short 'p'
      <> showDefault
      <> value 0.01
      <> help "probability of mutation" )
  <*> option auto
      (  long "crossover"
      <> short 'c'
      <> showDefault
      <> value 0.6
      <> help "probability of crossover" )
  <*> option auto
      (  long "individual-length"
      <> short 'l'
      <> showDefault
      <> value 100
      <> help "length of an individual" )
  <*> option auto
      (  long "population-size"
      <> short 'z'
      <> showDefault
      <> value 30
      <> help "size of the population" )
  <*> option auto
      (  long "generations"
      <> short 'g'
      <> showDefault
      <> value 100
      <> help "number of generations to run" )
  <*> switch
      (  long "poly"
      <> short 'y'
      <> help "Run polymorphic GA" )
  <*> switch
      (  long "mono"
      <> short 'm'
      <> help "Run monomorphic GA" )
  <*> switch
      (  long "mut"
      <> short 'u'
      <> help "Run mutable GA" )

polypop :: Int -> Int -> Rand IO (V.Vector (S.Seq Word32))
polypop ps is = V.replicateM ps . S.replicateM (is `div` 32) $ MWC.uniform
monoFitness ind = fromIntegral . F.sum . fmap (fromIntegral . popCount) $ ind
polyFitness ind = fromIntegral . F.sum . fmap (fromIntegral . popCount) $ ind

printPop :: (Foldable f, Foldable g, Functor f, Functor g) => f (g Word32) -> String
printPop p = L.intercalate "\n" $ fmap (L.intercalate " ") $ (fmap . fmap) (printf "%03X") $ F.toList $ fmap F.toList p

printPop32 :: (Foldable f, Foldable g, Functor f, Functor g) => f (g Word32) -> String
printPop32 p = L.intercalate "\n" $ fmap (L.intercalate " ") $ (fmap . fmap) (printf "%08X") $ F.toList $ fmap F.toList p

polyGA :: GaConfig -> IO ()
polyGA config = 
  do p <- MWC.runWithCreate $ Poly.standardGeneticAlgorithm (gaConfigPm config)
                                                            (gaConfigPc config)
                                                            (gaConfigPs config)
                                                            (gaConfigIs config)
                                                            (gaConfigGens config)
                                                            polypop
                                                            polyFitness
     putStrLn $ printPop p
     print $ snd $ F.maximumBy (compare `on` snd) $ evaluation polyFitness p


monoGA :: GaConfig -> IO ()
monoGA config =
  do p <- MWC.runWithCreate $ Mono.standardGeneticAlgorithm (gaConfigPm config)
                                                            (gaConfigPc config)
                                                            (gaConfigPs config)
                                                            (gaConfigIs config)
                                                            (gaConfigGens config)
                                                            monoBitPopulation
                                                            monoFitness
     putStrLn $ printPop32 p
     print $ snd $ F.maximumBy (compare `on` snd) $ evaluation monoFitness p

mutGA :: GaConfig -> IO ()
mutGA config =
  do p <- MWC.runWithCreate $ Mut.standardGeneticAlgorithmBits (gaConfigPm config)
                                                               (gaConfigPc config)
                                                               (gaConfigPs config)
                                                               (gaConfigIs config)
                                                               (gaConfigGens config)
                                                               iovectorBitCount
     --V.mapM VU.freeze p >>= print 
     p' <- runWithCreate $ evaluationM iovectorBitCount p
     printIOVectorPop p
     print $ snd $ F.maximumBy (compare `on` snd) p'
  
-- run a genetic algorithm. fitness is sum of loci
main :: IO ()
main = do
  config <- execParser $ info (cmdArgs <**> helper)
                              (  fullDesc
                              <> progDesc "Run a genetic algorithm"
                              <> header "a functional genetic algorithm program")
  when (gaConfigPoly config) $ polyGA config

  when (gaConfigMono config) $ monoGA config

  when (gaConfigMut config)  $ mutGA config

