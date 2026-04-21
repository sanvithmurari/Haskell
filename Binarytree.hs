--BINARY TREE IN HASKELL WITH FIUNCTIONS

data BTree a = Nil | Node (BTree a) a (BTree a)

tree1,tree2 :: BTree Integer
tree1= Node (Node (Node Nil 5 Nil) 3 (Node Nil 6 Nil)) 2 (Node Nil 4 Nil)
tree2=Node(Node (Node Nil 40 (Node Nil 60 Nil)) 20 Nil) 10 (Node Nil 30 (Node (Node Nil 70 Nil) 50 Nil)) 

isempty :: BTree a -> Bool
isempty Nil=True
isempty _=False

size :: BTree a -> Int
size Nil =0
size (Node tl x tr)=1+ size tl + size tr
