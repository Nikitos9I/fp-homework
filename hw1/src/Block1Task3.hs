module Block1Task3
  ( TreeList(..)
  , Tree(..)
  , isEmpty
  , countSize
  , findElem
  , insertElem
  , fromList
  , removeElem
  ) where

-- | List of tree data structure, contains tree elements
data TreeList a
  = One a
  | Many a (TreeList a)
  deriving (Show)

instance Eq a => Eq (TreeList a) where
  (==) (One a) (One b)         = a == b
  (==) (Many _ _) (One _)      = False
  (==) (One _) (Many _ _)      = False
  (==) (Many a l1) (Many b l2) = a == b && l1 == l2

get :: TreeList a -> a
get (One a)    = a
get (Many a _) = a

listSize :: TreeList a -> Int
listSize (One _)       = 1
listSize (Many _ list) = 1 + listSize list

contains :: Eq a => a -> TreeList a -> Bool
contains element (One a)    = element == a
contains element (Many a _) = element == a

-- | Tree data structure
data Tree a
  = Leaf
  | Node (TreeList a) (Tree a) (Tree a)
  deriving (Show)

instance Eq a => Eq (Tree a) where
  (==) Leaf Leaf = True
  (==) Leaf Node {} = False
  (==) Node {} Leaf = False
  (==) (Node l1 lt1 rt1) (Node l2 lt2 rt2) =
    l1 == l2 && lt1 == lt2 && rt1 == rt2

-- | Check if tree is empty
-- @param - Tree
-- @return - True if tree is empty or False if not
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Count size of tree (size of node = size of elements in it's list)
-- @param - Tree
-- @return - Int with tree size
countSize :: Tree a -> Int
countSize Leaf = 0
countSize (Node elems leftChild rightChild) =
  listSize elems + countSize leftChild + countSize rightChild

-- | Find element in tree
-- @param - element with Ord instance, Tree
-- @return - Nothing of no element in tree or Just with subTree
findElem :: Eq a => a -> Tree a -> Maybe (Tree a)
findElem _ Leaf = Nothing
findElem element (Node list leftChild rightChild) =
  case contains element list of
    True -> Just (Node list leftChild rightChild)
    False ->
      case findElem element leftChild of
        Just just -> Just just
        Nothing   -> findElem element rightChild

-- | Insert element to tree
-- @param - element with Ord instance, Tree
-- @return - new tree with inserted element
insertElem :: Ord a => a -> Tree a -> Tree a
insertElem element Leaf = Node (One element) Leaf Leaf
insertElem element (Node list leftChild rightChild) =
  case contains element list of
    True -> Node (Many element list) leftChild rightChild
    False ->
      case element < get list of
        True  -> Node list (insertElem element leftChild) rightChild
        False -> Node list leftChild $ insertElem element rightChild

-- | Get tree from list
-- @param - list with elements with Ord instance
-- @return - tree
fromList :: Ord a => [a] -> Tree a
fromList = foldr insertElem Leaf . reverse

removeHelper :: Tree a -> Tree a -> Tree a
removeHelper Leaf tree             = tree
removeHelper (Node l ltc rtc) tree = Node l ltc $ removeHelper rtc tree

-- | Remove element from tree
-- @param - element with Ord instance, Tree
-- @return - if tree contains element then new tree with removed element
-- else source tree
removeElem :: Ord a => a -> Tree a -> Tree a
removeElem _ Leaf = Leaf
removeElem element (Node list leftChild rightChild) =
  case contains element list of
    True ->
      case list of
        One _       -> removeHelper leftChild rightChild
        Many _ rest -> Node rest leftChild rightChild
    False ->
      case element < get list of
        True  -> Node list (removeElem element leftChild) rightChild
        False -> Node list leftChild $ removeElem element rightChild
