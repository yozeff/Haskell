--Joseph Harrison 2019
--binary search tree implementation
--with algebraic data types

data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Read, Eq, Show)

--functions can be mapped over trees
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

--makes a tree with one node
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

--create new nodes and expand tree
tree_insert :: (Ord a) => a -> Tree a -> Tree a
--edge condition create new node
tree_insert x EmptyTree = singleton x
--continue traversing
tree_insert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (tree_insert x left) right
    | x > a  = Node a left (tree_insert x right)

--check if an element is in tree
tree_elem :: (Ord a) => a -> Tree a -> Bool
--if we have reached a terminating node
tree_elem x EmptyTree = False
--continue traversing
tree_elem x (Node a left right)
    | x == a = True
    | x < a  = tree_elem x left
    | x > a  = tree_elem x right

main = do
    let nums = [8, 6, 4, 1, 7, 3, 5]
    let numstree = foldl (\acc x -> tree_insert x acc) EmptyTree nums
    putStrLn $ show numstree
    putStrLn $ show $ 8 `tree_elem` numstree
    putStrLn $ show $ 9 `tree_elem` numstree
    putStrLn "mapping multiplication by 3"
    putStrLn $ show $ fmap (\x -> "abcdef" !! (x `mod` 6)) numstree
    putStrLn "done"

