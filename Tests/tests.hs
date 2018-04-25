{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.QuickCheck.Modifiers

import qualified Data.Vector as V
import qualified Data.Sequence as S
import Data.Tree

import Data.Foldable as F
import Data.List.Split
import Data.Word

import System.Random.MWC.Monad

import Heal.Polymorphic as Poly
import Heal.Monomorphic as Mono
import Heal.Scope


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcUtility, qcMutation, qcCrossover]

population :: V.Vector (Int, Double)
population = V.zip [0..] [1.0, 2.0, 3.0, 5.0]

repopulateVector :: V.Vector Int -> [Int] -> V.Vector Int
repopulateVector = repopulate

repopulateTree :: Tree Int -> [Int] -> Tree Int
repopulateTree = repopulate

balancedTree :: [Int] -> (Int, [[Int]])
balancedTree [] = (0, [])
balancedTree (ix:[]) = (ix, [])
balancedTree (ix:ixs) = (ix, chunksOf (length ixs `div` 2) ixs)

posNonEmpty a = getNonEmpty . fmap getPositive $ a

-- equalLengthAfterSUS ::  [(Int, Double)] -> IO Bool
-- equalLengthAfterSUS p =
--   runWithCreate $ do
--     p' <- susPoly $ V.fromList p
--     return $ length p == V.length p'

{- QuickCheck Tests -}
qcUtility = testGroup "QuickCheck Utility Functions"
  [ QC.testProperty "Repopulate Uses All Elements (Vector)"
    (\list -> list == F.toList (repopulateVector (V.fromList list) list))

  , QC.testProperty "Teeth Preserves Size"
    (\a -> let (list :: [Double]) = posNonEmpty a in
               any (<1) list || length (teeth 0 (V.fromList $ zip list list)) == length list)

  , QC.testProperty "Comb Preserves Size"
    (\(ds) -> let list :: [Double] = posNonEmpty ds
              in V.length (comb 0.0 (V.fromList (zip list list))) == length list)
  ]

-- qcSelection = testGroup "Quickcheck Selection Properties"
--   [ QC.testProperty "SUS Selection Preserves Length" $
--     (\p -> ioProperty $ equalLengthAfterSUS p)
--   ]

qcMutation = testGroup "Quickcheck Mutation Properties"
  [ QC.testProperty "Mutation Preserves Length" $
    (\(p :: [Bool]) -> ioProperty $ do
      p' <- runWithCreate (pointMutationLociStd 1 p)
      return $ (length p == length p'))

  , QC.testProperty "Mutation Modifies" $
    (\(p :: [Double]) -> ioProperty $ do
      p' <- runWithCreate (pointMutationLociStd 1 p)
      return $ (length p == 0 || (p /= p')))

  , QC.testProperty "Mutation 0 Prob Does not Change" $
    (\(p :: [Double]) -> ioProperty $ do
      p' <- runWithCreate (pointMutationLociStd 0 p)
      return $ (p == p'))
  ]
 
qcCrossover = testGroup "Quickcheck Crossover Properties"
  [ QC.testProperty "Crossover Preserves Lengths" $
    (\((point, list, list') :: (Int, NonEmptyList Bool, NonEmptyList Bool)) -> 
      let point' = point `mod` (max (length as) (length bs))
          (as, bs) = (getNonEmpty list, getNonEmpty list')
          (as', bs') = Poly.cross point' as bs
       in (length as + length bs) == (length as' + length bs'))

  , QC.testProperty "Crossover Preserves Elements" $
    (\((point, list, list') :: (Int, NonEmptyList Bool, NonEmptyList Bool)) -> 
      let point' = point `mod` (max (length as) (length bs))
          (as, bs) = (getNonEmpty list, getNonEmpty list')
          (as', bs') = Poly.cross point' as bs
       in all (\a -> a `elem` as || a `elem` bs) bs && 
          all (\a -> a `elem` as || a `elem` bs) as)

  ]

{- Unit Tests -}
unitTests = testGroup "Unit tests"
  [ testCase "SUS Comb Function Test Case 1" $
    let is = V.zip [0,1,2,3] [1, 2, 3, 50]
    in comb 0.0 is @?= [0, 3, 3, 3]

  , testCase "Cross and CrossAligned Agree" $
    let is = [0.100]
     in Poly.cross 50 is (reverse is) @?= crossAligned 50 is (reverse is)

  , indexerTests 
  ]

indexerTests = testGroup "Indexer Tests"
  [ utSeqIndexer
  , utBitIndexer
  , utListIndexer 
  , utVectIndexer 
  , utPackedBitsScope
  ]

utPackedBitsScope = testGroup "Packed Bits Indexer Test"
  [
    testCase "into BitsPopulation Indexer move/replace test" $
      let sIx = into packedBitsScope $ V.replicate 2 $ S.replicate 5 0
      in outOf packedBitsScope (replace packedBitsScope True . move packedBitsScope (Just 1, (Just 4, 2)) $ sIx) @?= [[0, 0, 0, 0, 0], [0, 0, 0, 0, 4]]

  , testCase "into BitsPopulation Indexer move/replace test 2" $
      let sIx = into packedBitsScope $ V.replicate 2 $ S.replicate 5 0
      in outOf packedBitsScope (replace packedBitsScope True . move packedBitsScope (Nothing, (Nothing, 2)) . replace packedBitsScope True . move packedBitsScope (Just 1, (Just 4, 2)) $ sIx) @?= [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0x14]]
  ]

utSeqIndexer = testGroup "Seq Indexer Test"
  [ testCase "into Seq Indexer move/replace test" $
    let sIx = into seqIndexer [1..9]
     in outOf seqIndexer (move seqIndexer 2 . replace seqIndexer 100 . move seqIndexer 2 $ sIx) @?= [1, 2, 100, 4, 5, 6, 7, 8, 9]

  , testCase "Seq Indexer move/replace at end test" $
    let sIx = into seqIndexer [1..9]
     in outOf seqIndexer (move seqIndexer 4 . replace seqIndexer 100 . move seqIndexer 100 $ sIx) @?= [1, 2, 3, 4, 5, 6, 7, 8, 100]

  , testCase "Seq Indexer set/get test" $
    let sIx = into seqIndexer [1..9 :: Int]
     in (get seqIndexer. replace seqIndexer 100 $ sIx) @?= 100

  , testCase "Seq Indexer move/get test" $
    let sIx = into seqIndexer [1..9 :: Int]
     in (get seqIndexer . move seqIndexer 5 $ sIx) @?= 6
  ]

utBitIndexer = testGroup "Bit Indexer Test"
  [ testCase "Bit Indexer move/replace test" $
    let bIx = into bitsIndexer (0xA0 :: Word8)
     in outOf bitsIndexer (replace bitsIndexer True . move bitsIndexer 2 $ bIx) @?= 0xA4

  , testCase "Bit Indexer move to end test" $
    let bIx = into bitsIndexer (0x00 :: Word16)
     in outOf bitsIndexer (replace bitsIndexer True . move bitsIndexer 100 $ bIx) @?= 0x8000

  , testCase "Bit Indexer move to front test" $
    let bIx = into bitsIndexer (0x00 :: Word16)
     in outOf bitsIndexer (replace bitsIndexer True . move bitsIndexer (-10) . move bitsIndexer 5 $ bIx) @?= 0x1
  ]

utListIndexer = testGroup "List Indexer Test"
  [ testCase "List Indexer move/replace test" $
    let sIx = into listIndexer [1..9]
     in outOf listIndexer (move listIndexer 4 . replace listIndexer 100 . move listIndexer 2 $ sIx) @?= [1, 2, 100, 4, 5, 6, 7, 8, 9]

  , testCase "List Indexer move/replace at end test" $
    let sIx = into listIndexer [1..9]
     in outOf listIndexer (move listIndexer 4 . replace listIndexer 100 . move listIndexer 100 $ sIx) @?= [1, 2, 3, 4, 5, 6, 7, 8, 100]

  , testCase "List Indexer set/get test" $
    let sIx = into listIndexer [1..9 :: Int]
     in (get listIndexer . replace listIndexer 100 $ sIx) @?= 100

  , testCase "List Indexer move/get test" $
    let sIx = into listIndexer [1..9 :: Int]
     in (get listIndexer . move listIndexer 5 $ sIx) @?= 6
  ]

utVectIndexer = testGroup "Vect Indexer Test"
  [ testCase "Vect Indexer move/replace test" $
    let sIx = into listIndexer [1..9]
     in outOf listIndexer (move listIndexer 4 . replace listIndexer 100 . move listIndexer 2 $ sIx) @?= [1, 2, 100, 4, 5, 6, 7, 8, 9]

  , testCase "Vect Indexer move/replace at end test" $
    let sIx = into listIndexer [1..9]
     in outOf listIndexer (move listIndexer 4 . replace listIndexer 100 . move listIndexer 100 $ sIx) @?= [1, 2, 3, 4, 5, 6, 7, 8, 100]

  , testCase "Vect Indexer set/get test" $
    let sIx = into listIndexer [1..9 :: Int]
     in (get listIndexer . replace listIndexer 100 $ sIx) @?= 100

  , testCase "Vect Indexer move/get test" $
    let sIx = into listIndexer [1..9 :: Int]
    in (get listIndexer . move listIndexer 5 $ sIx) @?= 6
  ]
