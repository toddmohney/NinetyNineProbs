module NinetyNineProbs where
  import Data.List
  import Data.Tree

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


  data BTree a = Empty
              | Branch a (BTree a) (BTree a)
              deriving Show

  leaf :: a -> BTree a
  leaf a = Branch a Empty Empty

  balBTree :: Int -> BTree Char
  balBTree 0 = Empty
  balBTree 1 = insertBalanced Empty (leaf 'x')
  balBTree n = balBTree' (leaf 'x') (n-1)
    where 
      balBTree' :: BTree Char -> Int -> BTree Char
      balBTree' tree n
        | n > 0 = balBTree' (insertBalanced tree (leaf 'x')) (n-1)
        | otherwise = tree

  -- fix this.
  insertBalanced :: BTree a -> BTree a -> BTree a
  insertBalanced Empty node = node
  insertBalanced (Branch v Empty r) node = Branch v node r
  insertBalanced (Branch v l Empty) node = Branch v l node
  insertBalanced (Branch v l r) node
    | treeDepth l > treeDepth r      = Branch v l (insertBalanced r node) 
    | otherwise                      = Branch v (insertBalanced l node) r

  treeDepth :: BTree a -> Int
  treeDepth Empty = 0
  treeDepth (Branch _ Empty Empty) = 1
  treeDepth (Branch _ l r)
    | treeDepth l > treeDepth r = 1 + treeDepth l
    | otherwise                 = 1 + treeDepth r


  symmetric :: BTree a -> Bool
  symmetric Empty = True
  symmetric (Branch _ a b) = mirror a b

  mirror :: BTree a -> BTree a -> Bool
  mirror Empty Empty                   = True
  mirror (Branch _ a b) (Branch _ c d) = mirror a d && mirror b c
  mirror _ _                           = False

  -- constructs a binary tree
  construct :: (Ord a) => [a] -> BTree a
  construct []   = Empty
  construct list = foldl (flip add) Empty list

  add :: (Ord a) => a -> BTree a -> BTree a
  add x Empty = leaf x
  add x (Branch v l r)
    | x < v     = Branch v (add x l) r
    | otherwise = Branch v l (add x r)

  countLeaves :: BTree a -> Int
  countLeaves Empty = 0
  countLeaves (Branch _ Empty Empty) = 1
  countLeaves (Branch _ l r) = countLeaves l + countLeaves r

  leaves :: BTree a -> [a]
  leaves Empty = []
  leaves (Branch v Empty Empty) = [v]
  leaves (Branch _ l r) = leaves l ++ leaves r

  internals :: BTree a -> [a]
  internals Empty = []
  internals (Branch v Empty Empty) = []
  internals (Branch v l r) = [v] ++ internals l ++ internals r

  atLevel :: BTree a -> Int -> [a]
  atLevel Empty _ = []
  atLevel (Branch v l r) level
    | level == 1 = [v]
    | level > 1 = atLevel l (level-1) ++ atLevel r (level-1)
    | otherwise = []

  isCompleteBinaryBTree :: BTree a -> Bool
  isCompleteBinaryBTree Empty = False
  isCompleteBinaryBTree (Branch _ Branch{} Empty ) = False
  isCompleteBinaryBTree (Branch _ Empty Branch{}) = False
  isCompleteBinaryBTree (Branch _ Empty Empty) = True
  isCompleteBinaryBTree (Branch _ l@Branch{} r@Branch{}) = isCompleteBinaryBTree l && isCompleteBinaryBTree r


