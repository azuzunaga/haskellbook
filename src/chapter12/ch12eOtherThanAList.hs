module SomethingOtherThanAList where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f t = case f t of
  Nothing               -> Leaf
  Just (left, x, right) -> Node (unfold f left) x (unfold f right)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (builder n) (-1)

builder :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
builder d c | d == c + 1 = Nothing
builder d c              = Just (c + 1, c + 1, c + 1)

addN :: BinaryTree Int -> Int -> BinaryTree Int
addN Leaf                _ = Leaf
addN (Node left x right) n = Node (addN left n) (x + n) (addN right n)

addOne :: BinaryTree Int -> BinaryTree Int
addOne = flip addN 1

data RoseTree a = Nil | Bud a [RoseTree a]

addOne' :: RoseTree Int -> RoseTree Int
addOne' Nil        = Nil
addOne' (Bud n xs) = Bud (n + 1) (map addOne' xs)

treeBuild' :: Integer -> BinaryTree Integer
treeBuild' n = go n Leaf (-1)
 where
  go depth tree counter | depth == counter + 1 = tree
  go depth tree counter = Node (go depth tree (counter + 1))
                               (counter + 1)
                               (go depth tree (counter + 1))
