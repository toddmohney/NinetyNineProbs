module NinetyNineProbs where
  import Data.List

  myLast :: [a] -> a
  myLast []   = error "*** error: myLast is undefined for an empty list"
  myLast [a]  = a
  myLast list = head . reverse $ list

  myButLast :: [a] -> a
  myButLast []          = error "*** error: myButLast is undefined when the lenght of the list is < 2"
  myButLast [x]      = error "*** error: myButLast is undefined when the lenght of the list is < 2"
  myButLast list@(x:xs) = myLast . take 2 . reverse $ list

  elementAt :: [a] -> Int -> a
  elementAt list index
    | length list < index = error "*** error: the index is greater than the lenght of the list"
    | otherwise = myLast . take index $ list

  myLength :: [a] -> Int
  myLength = foldl (\acc _ -> acc + 1) 0

  myReverse :: [a] -> [a]
  myReverse = foldr (\x acc -> acc ++ [x]) []

  isPalendrome :: (Eq a) => [a] -> Bool
  isPalendrome list = list == myReverse list

  data NestedList a = Elem a 
                    | List [NestedList a]

  myFlatten :: NestedList a -> [a]
  myFlatten (Elem x)      = [x]
  myFlatten (List [])     = []
  myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

  compress :: (Eq a) => [a] -> [a]
  compress [] = []
  compress [x] = [x]
  compress (x:y:xs) 
    | x == y    = compress (y:xs)
    | otherwise = x : compress (y:xs)

  pack :: (Eq a) => [a] -> [[a]]
  pack [] = []
  pack (x:xs) = let (grouped, rest) = span (==x) xs in
                    (x:grouped) : pack rest

  encode :: String -> [(Int,Char)]
  encode [] = []
  encode list@(x:xs) = map encode' $ pack list
    where 
      encode' :: String -> (Int,Char)
      encode' xs = (length xs, head xs)


  data RunLength a = Multiple Int Char
                   | Single Char
                   deriving Show

  encodeModified :: String -> [RunLength Char]
  encodeModified list = map buildRunLength (encode list)
    where 
      buildRunLength :: (Int,Char) -> RunLength Char
      buildRunLength (1,a) = Single a
      buildRunLength (n,a) = Multiple n a


  decodeModified :: [RunLength Char] -> String
  decodeModified []     = ""
  decodeModified [x]    = decompress x
  decodeModified (x:xs) = decompress x ++ decodeModified xs


  decompress :: RunLength Char -> String
  decompress (Single a) = [a]
  decompress (Multiple n e) = repli [e] n


  repli :: (Enum a) => [a] -> Int -> [a]
  repli [] _     = []
  repli [x] c    = take c [x,x..]
  repli (x:xs) c = repli [x] c ++ repli xs c

  dupli :: (Enum a) => [a] -> [a]
  dupli = flip repli 2


