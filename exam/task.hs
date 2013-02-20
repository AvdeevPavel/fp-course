import Control.Monad.Writer

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Read)

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Crumbs a = [Crumb a]

type Zipper a = (Tree a, Crumbs a)

-- Глобально мы хотим ведь a ++ (b ++ (c ++ (d ++ (e ++ f)))). Фактически разностные списки (1)
-- alpha - version
flatten :: (Show a) => Tree a -> String
flatten t = flatten' (t, [])

flatten' :: (Show a) => Zipper a -> String
flatten' all@(Leaf, xs) = goUp' all
flatten' (Node x ls rs, bs) = flatten' (ls, (LeftCrumb x rs):bs)

goUp' :: (Show a) => Zipper a -> String
goUp' (t, (LeftCrumb x r):bs) = (show x) ++ (flatten' (r, []) ++ goUp' (Node x t r, bs) )
goUp' (_, []) = ""

-- beta - version. Прям та сигнатура функции, которую ты говорил
flatten1 :: (Show a) => Tree a -> String
flatten1 t = flatten'' t []

flatten'' :: (Show a) => Tree a -> [(String, Tree a)] -> String
flatten'' Leaf all = goUp'' all
flatten'' (Node x ls rs) all = flatten'' ls (((show x), rs):all)

goUp'' :: (Show a) => [(String, Tree a)] -> String
goUp'' ((x, rs):xs) = x ++ (flatten'' rs [] ++ goUp'' xs) 
goUp'' [] = ""

-- 
-- Разностные списки есть в Learn you a Haskell for Great Good. Эту идею на сдаче не предлагал, но она витала в голове.   
--
--newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}
type DiffList a = [a] -> [a]
 
toDiffList :: [a] -> DiffList a 
toDiffList xs = (\ys -> xs ++ ys) --DiffList (xs++) 

fromDiffList :: DiffList a -> [a]
fromDiffList f = f []  --fromDiffList DiffList f = f []  

getString :: (Show a) => Tree a -> String 
getString = fromDiffList . getString' . convertToStr

convertToStr :: (Show a) => Tree a -> Tree String 
convertToStr Leaf = Leaf
convertToStr (Node x ls rs) = Node (show x) (convertToStr ls) (convertToStr rs) 

getString' :: Tree [a] -> DiffList a 
getString' Leaf = toDiffList [] 
getString' (Node x ls rs) = (getString' ls) . (toDiffList x) . (getString' rs)

--Это я писал тебе на листике. Первый вариант, через свертку. Могу показать. Листик лежит рядом с тобой.  
--Был не много другой порядок. Но он был с concat за линию. 
tfold :: (a -> b -> b) -> b -> Tree a -> b 
tfold _ acc Leaf = acc
tfold f acc (Node x ls rs) = f x $ tfold f (tfold f acc rs) ls  

tfoldc :: (a -> b -> b) -> b -> Tree a -> b 
tfoldc _ acc Leaf = acc 
tfoldc f acc (Node x ls rs) = tfoldc f (f x $ tfoldc f acc rs) ls

--Конкат в даже я реализовывал через фолдр и это будет линия, так как (1)    
--То что было на листике с другим порядком
getStr :: (Show a) => Tree a -> String
getStr t = concat . tfold (\x acc -> (show x):acc) [] $ t

--Еще один getStr только что бы уже споров не было в нормальном порядке. 
getStr' :: (Show a) => Tree a -> String
getStr' t = concat . tfoldc (\x acc -> (show x):acc) [] $ t

-- Вариант на merge с реверсом. Писал на листике. В ней была одна маленькая ошибка. Я почему то исопльзовал свой merge, а не фолдр
myString :: (Show a) => Tree a -> String -> String
myString Leaf acc = acc 
myString (Node x ls rs) acc = myString ls (foldr (\x acc -> x: acc) (myString rs acc) (show x)) 	
	
test = Node 'x' (Node 'a' Leaf Leaf) (Node 'b' Leaf Leaf) 
test1 = Node 'x' (Node 'a' Leaf (Node 'c' Leaf Leaf)) (Node 'b' Leaf Leaf)
test2 = Node 'x' (Node 'a' Leaf (Node 'c' (Node 'e' Leaf Leaf) (Node 'g' Leaf Leaf))) (Node 'b' Leaf Leaf)
test3 = Node "x" (Node "ab" Leaf (Node "c" (Node "e" Leaf Leaf) (Node "g" Leaf Leaf))) (Node "b" Leaf Leaf)  
  
-- Написал, но это работает за квадрат все равно. Ну за квадрат ведь. 
-- Не понимаю как с твоей идеей, в порядке l root right надо вывести за линию.  
{-
flatten1 :: (Show a) => Tree a -> String
flatten1 t = flatten1' t ([], Leaf) 

flatten1' :: (Show a) => Tree a -> ([String], Tree a) -> String
flatten1' Leaf (x:_, Leaf) = x
flatten1' Leaf (all@(x:_), (Node c ls rs)) = x ++ flatten1' ls ((show c):all, rs)
flatten1' (Node c ls rs) ([], _) = flatten1' ls ((show c):[], rs)  
flatten1' (Node c ls rs) (all@(x:_), (Node tc tls trs)) = flatten1' ls ((show c):all, rs) ++ (x ++ flatten1' tls ((show tc):all, trs))   
-}

