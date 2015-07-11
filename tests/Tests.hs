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

  {- Not a complete test -}
  {- We are not testing the empty list here -}
  {- myLast, however cannot handle an empty list anyway -}
  testMyLast :: [Int] -> Bool
  testMyLast list = myLast (list ++ [123]) == 123

  testMyButLast :: [Int] -> Bool
  testMyButLast list = myButLast (list ++ [123, 456]) == 123

  {- testElementAt :: Int -> Bool -}
  {- testElementAt index = (elementAt [1..1000] index) == index -}

  testMyLength :: [Int] -> Bool
  testMyLength list = (myLength list) == (length list)

  testMyReverse :: [Int] -> Bool
  testMyReverse list = (myReverse list) == (reverse list)

  tests :: TestTree
  tests = testGroup "Tests" [properties, unitTests]

  properties :: TestTree
  properties = testGroup "Properties" [qcProps]

  qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "sort == sort . reverse" $ testSort
    , QC.testProperty "Fermat's little theorem" $ testFermat
    , QC.testProperty "myLast" $ testMyLast
    , QC.testProperty "myButLast" $ testMyButLast
    {- , QC.testProperty "MyElementAt" $ testElementAt -}
    , QC.testProperty "myLength" $ testMyLength
    , QC.testProperty "myReverse" $ testMyReverse
    ]

  unitTests = testGroup "Unit tests"
    [ testCase "List comparison (different length)" $ [1, 2, 3] `compare` [1,2] @?= GT
    ]
