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


  data Tree a = Empty
              | Branch a (Tree a) (Tree a)
              deriving Show

  leaf :: a -> Tree a
  leaf a = Branch a Empty Empty

  balTree :: Int -> Tree Char
  balTree 0 = Empty
  balTree 1 = insertBalanced Empty (leaf 'x')
  balTree n = balTree' (leaf 'x') (n-1)
    where 
      balTree' :: Tree Char -> Int -> Tree Char
      balTree' tree n
        | n > 0 = balTree' (insertBalanced tree (leaf 'x')) (n-1)
        | otherwise = tree

  -- fix this.
  insertBalanced :: Tree a -> Tree a -> Tree a
  insertBalanced Empty node = node
  insertBalanced (Branch v Empty r) node = Branch v node r
  insertBalanced (Branch v l Empty) node = Branch v l node
  insertBalanced (Branch v l r) node
    | treeDepth l > treeDepth r      = Branch v l (insertBalanced r node) 
    | otherwise                      = Branch v (insertBalanced l node) r

  treeDepth :: Tree a -> Int
  treeDepth Empty = 0
  treeDepth (Branch _ Empty Empty) = 1
  treeDepth (Branch _ l r)
    | treeDepth l > treeDepth r = 1 + treeDepth l
    | otherwise                 = 1 + treeDepth r


  symmetric :: Tree a -> Bool
  symmetric Empty = True
  symmetric (Branch _ a b) = mirror a b

  mirror :: Tree a -> Tree a -> Bool
  mirror Empty Empty                   = True
  mirror (Branch _ a b) (Branch _ c d) = mirror a d && mirror b c
  mirror _ _                           = False

  -- constructs a binary tree
  construct :: (Ord a) => [a] -> Tree a
  construct []   = Empty
  construct list = foldl (flip add) Empty list

  add :: (Ord a) => a -> Tree a -> Tree a
  add x Empty = leaf x
  add x (Branch v l r)
    | x < v     = Branch v (add x l) r
    | otherwise = Branch v l (add x r)

  countLeaves :: Tree a -> Int
  countLeaves Empty = 0
  countLeaves (Branch _ Empty Empty) = 1
  countLeaves (Branch _ l r) = countLeaves l + countLeaves r

  leaves :: Tree a -> [a]
  leaves Empty = []
  leaves (Branch v Empty Empty) = [v]
  leaves (Branch _ l r) = leaves l ++ leaves r

  internals :: Tree a -> [a]
  internals Empty = []
  internals (Branch v Empty Empty) = []
  internals (Branch v l r) = [v] ++ internals l ++ internals r

  atLevel :: Tree a -> Int -> [a]
  atLevel Empty _ = []
  atLevel (Branch v l r) level
    | level == 1 = [v]
    | level > 1 = atLevel l (level-1) ++ atLevel r (level-1)
    | otherwise = []

  isCompleteBinaryTree :: Tree a -> Bool
  isCompleteBinaryTree Empty = False
  isCompleteBinaryTree (Branch _ Branch{} Empty ) = False
  isCompleteBinaryTree (Branch _ Empty Branch{}) = False
  isCompleteBinaryTree (Branch _ Empty Empty) = True
  isCompleteBinaryTree (Branch _ l@Branch{} r@Branch{}) = isCompleteBinaryTree l && isCompleteBinaryTree r


