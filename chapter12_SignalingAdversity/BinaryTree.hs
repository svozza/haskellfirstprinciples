data BinaryTree a = 
     Leaf
  |  Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = go f x Leaf
  where 
    go f x tree = case f x of
      Nothing         -> tree
      Just (y, z, y') -> Node (go f y tree) z (go f y' tree)

treebuild :: Integer -> BinaryTree Integer
treebuild n = unfold f 0
  where
    f x = if x > n - 1
          then Nothing
          else Just (x + 1, n, x + 1)
