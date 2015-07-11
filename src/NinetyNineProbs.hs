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

  elementAt :: [a] -> Int -> a
  elementAt list index
    | (length list) < index = error "*** error: the index is greater than the lenght of the list"
    | otherwise = myLast . take index $ list

  myLength :: [a] -> Int
  myLength = foldl (\acc _ -> acc + 1) 0

  myReverse :: [a] -> [a]
  myReverse = foldr (\x acc -> acc ++ [x]) []

  isPalendrome :: (Eq a) => [a] -> Bool
  isPalendrome list = list == (myReverse list)

  data NestedList a = Elem a 
                    | List [NestedList a]

  myFlatten :: NestedList a -> [a]
  myFlatten (Elem x)      = [x]
  myFlatten (List [])     = []
  myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

  compress :: (Eq a) => [a] -> [a]
  compress [] = []
  compress (x:[]) = [x]
  compress (x:y:xs) 
    | x == y    = compress (y:xs)
    | otherwise = [x] ++ compress (y:xs)

  pack :: (Eq a) => [a] -> [[a]]
  pack [] = []
  pack (x:xs) = let (grouped, rest) = span (==x) xs in
                    (x:grouped):(pack rest)

  encode :: (Eq a) => [a] -> [(Int,a)]
  encode [] = []
  encode list@(x:xs) = map encode' $ pack list
    where 
      encode' :: [a] -> (Int,a)
      encode' xs = ((length xs), (head xs))
