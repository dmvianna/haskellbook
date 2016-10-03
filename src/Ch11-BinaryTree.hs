import qualified Data.Foldable as F

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

--


preorder :: BinaryTree a -> [a]
preorder x = go x []
    where go tree acc =
              case tree of
                Node Leaf a Leaf -> a:acc
                Node left a right ->
                  a : (go left acc) ++ (go right acc)

  
inorder :: BinaryTree a -> [a]
inorder x = go x []
    where go tree acc =
              case tree of
                Node Leaf a Leaf -> a:acc
                Node left a right ->
                  (go left acc) ++ [a] ++ (go right acc)



postorder :: Ord a => BinaryTree a -> [a]
postorder x = go x []
    where go tree acc =
              case tree of
                Node Leaf a Leaf -> a:acc
                Node left a right ->
                  (go left acc) ++ (go right acc) ++ [a]


testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2,1,3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1,2,3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."


testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1,3,2]
  then putStrLn "Postorder fine!"
  else putStrLn "Postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf               = b
foldTree f b (Node Leaf a Leaf) = f a b
foldTree f b (Node l a r)       = foldTree f (f a (foldTree f b l)) r
