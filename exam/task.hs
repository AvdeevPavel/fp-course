import Control.Monad.Writer

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Read)

flatten' :: (Show a) => Tree a -> ([String], Tree a) -> String
flatten' Leaf (x:_, Leaf) = x
flatten' Leaf (all@(x:_), (Node c ls rs)) = x ++ flatten' ls ((show c):all, rs)
flatten' (Node c ls rs) ([], _) = flatten' ls ((show c):[], rs)  
flatten' (Node c ls rs) (all@(x:_), (Node tc tls trs)) = flatten' ls ((show c):all, rs) ++ (x ++ flatten' tls ((show tc):all, trs))   

test = Node 'x' (Node 'a' Leaf Leaf) (Node 'b' Leaf Leaf) 
test1 = Node 'x' (Node 'a' Leaf (Node 'c' Leaf Leaf)) (Node 'b' Leaf Leaf)
test2 = Node 'x' (Node 'a' Leaf (Node 'c' (Node 'e' Leaf Leaf) (Node 'g' Leaf Leaf))) (Node 'b' Leaf Leaf)  




