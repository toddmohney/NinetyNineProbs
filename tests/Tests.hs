module Main where
  import Test.Tasty
  import Test.Tasty.SmallCheck as SC
  import Test.Tasty.QuickCheck as QC
  import Test.Tasty.HUnit

  import Data.List
  import Data.Ord
  import NinetyNineProbs

  main = defaultMain tests

  testSort :: [Int] -> Bool
  testSort list = sort (list :: [Int]) == sort (reverse list)

  testFermat :: Integer -> Bool
  testFermat x = ((x :: Integer)^7 - x) `mod` 7 == 0

  tests :: TestTree
  tests = testGroup "Tests" [properties, unitTests]

  properties :: TestTree
  properties = testGroup "Properties" [qcProps]

  qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "sort == sort . reverse" $ testSort
    , QC.testProperty "Fermat's little theorem" $ testFermat
    ]

  unitTests = testGroup "Unit tests"
    [ testCase "List comparison (different length)" $ [1, 2, 3] `compare` [1,2] @?= GT
    ]