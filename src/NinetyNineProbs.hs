module NinetyNineProbs where
  import Data.List

  myLast :: [a] -> a
  myLast []   = error "No end for empty lists!"
  myLast [a]  = a
  myLast list = head . reverse $ list

