{-# LANGUAGE NoImplicitPrelude #-}
module List where

import Primitive
import Prelude (Show, Read, error)
import qualified Prelude as P ((++))





---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length = foldl (\acc _ -> Succ acc) Zero

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ bs = bs 
(Cons a as) ++ bs = Cons a (as ++ bs)

-- Список без первого элемента
tail :: List a -> List a
tail Nil = error "list.tail empty list"
tail (Cons _ xs) = xs

-- Список без последнего элемента
init :: List a -> List a
init Nil = error "list.init empty list"
init (Cons x Nil) = Nil
init (Cons x xs) = Cons x (init xs)

-- Первый элемент
head :: List a -> a
head Nil = error "list.head: empty list"
head (Cons x _) = x

-- Последний элемент
last :: List a -> a
last Nil = error "list.last: empty list"
last (Cons x Nil) = x
last (Cons x xs) = last xs

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero _ = Nil
take _ Nil = Nil
take (Succ n) (Cons x xs) = Cons x $ take n xs

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero _ = Nil 
drop _ Nil = Nil 
drop (Succ n) (Cons x xs) = drop n xs

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter f = foldr (\x acc -> if' (f x) (Cons x acc) acc) Nil

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter _ Nil = Nil 
gfilter f (Cons x xs) = case (f x) of 
	Just y -> Cons y $ gfilter f xs
	Nothing -> gfilter f xs  

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile f (Cons x xs) = if' (f x) (Cons x $ takeWhile f xs) (Nil) 

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile f all@(Cons x xs) =  if' (f x) (dropWhile f xs) (all) 

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span _ Nil = Pair Nil Nil
span f all@(Cons x xs) = if' (f x) (let (Pair as bs) = span f xs in Pair (Cons x as) bs) (Pair Nil all) 

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break f = span (not . f)

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons a as) !! Zero = a 
(Cons a as) !! (Succ n) = as !! n

-- удаляет n-ый элемент списка (считая с нуля)
delete :: Nat -> List a -> List a
delete _ Nil = Nil 
delete Zero (Cons x xs) = xs
delete (Succ n) (Cons x xs) = Cons x $ delete n xs
 
-- Список задом на перёд
reverse :: List a -> List a
reverse = foldl (\acc x -> (Cons x acc)) Nil

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = subsequences xs ++ map (Cons x) (subsequences xs)

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations Nil = Cons Nil Nil 
permutations (Cons x xs) = foldr (++) Nil (map (inserter x) (permutations xs))
	where inserter x xs = insert' (length xs)
	      insert' Zero = Cons (Cons x xs) Nil 
	      insert' all@(Succ n) = Cons (take all xs ++ Cons x Nil ++ drop all xs) (insert' n)

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' Nil = Cons Nil Nil
permutations' xs = concatMap (\n -> map (Cons $ xs !! n) (permutations' $ delete n xs)) (createList $ length xs)
	where createList n = createList' n Nil
	      createList' Zero acc = acc
	      createList' (Succ n) acc = createList' n (Cons n acc)

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat x = Cons x $ repeat x 

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl _ z Nil = z 
foldl f z (Cons x xs) = foldl f (f z x) xs  

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl _ z Nil = (Cons z Nil)
scanl f z (Cons x xs) = Cons (f z x) (scanl f (f z x) xs)

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ z Nil = z
foldr f z (Cons x xs) = f x $ foldr f z xs 

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr _ z Nil = (Cons z Nil)
scanr f z (Cons x xs) = Cons (f x $ head func) func
	where func = scanr f z xs

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f = foldr (\x acc -> Cons (f x) acc) Nil

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat = foldr (++) Nil

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap _ Nil = Nil
concatMap f (Cons x xs) = f x ++ concatMap f xs 

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip = zipWith Pair 

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ Nil _ = Nil
zipWith _ _ Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)
