module NinetyNineProbs where
  import Data.List

  myLast :: [a] -> a
  myLast []   = error "*** error: myLast is undefined for an empty list"
  myLast [a]  = a
  myLast list = head . reverse $ list

  myButLast :: [a] -> a
  myButLast []          = error "*** error: myButLast is undefined when the lenght of the list is < 2"
  myButLast (x:[])      = error "*** error: myButLast is undefined when the lenght of the list is < 2"
  myButLast list@(x:xs) = myLast . take 2 . reverse $ list

