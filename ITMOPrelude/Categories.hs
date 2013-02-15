{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

-- Реализовать для всего,
-- что только можно из
import Primitive
import List
import Tree
-- всевозможные инстансы для классов ниже

--------------------------------------------------------------------------------
-- Классы
-- Категория - это объект обладающа морфизмами (стрелками). Для него поддерживается единичный элемент и композиция функций. 
-- Законы id * f = f g * id = g
-- Ассоциативность - (f * g) * h = f * (g * h)
class Category cat where
    id  :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

-- Отображает одну категорию в другую. 
-- Для объекта a из X должны иметь всегда объект f(a) в Y.
-- Для каждой стрелки f в X мы должны иметь F(f) в Y
-- Законы fmap id = id, fmap (f . g) = fmap f . fmap g 
class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\_ -> mb)

--------------------------------------------------------------------------------
-- Инстансы писать сюда

instance Category (->) where
	id = \x -> x 
	(.) f g = \x -> f (g x)  

instance Functor Maybe where
	fmap _ Nothing = Nothing 
	fmap f (Just x) = Just $ f x 

instance Functor List where 
	fmap = map	

instance Functor Tree where
	fmap = tmap

instance Monad Maybe where
	return = Just	
	Nothing >>= _ = Nothing 
	Just x >>= f = f x 
	 
instance Monad List where
	return x = Cons x Nil
	xs >>= f = concatMap f xs

instance Monad Tree where
	return x = Node x Leaf Leaf
	xs >>= f = tconcatMap f xs


--------------------------------------------------------------------------------
-- Монада State
newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
	return x = State $ \s -> (s, x)
	(State h) >>= f = State $ \s -> let (s', a) = h s
					    (State g) = f a
					in g s'
