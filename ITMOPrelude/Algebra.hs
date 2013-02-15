{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPredule.Algebra where

-- Реализовать для всего,
-- что только можно из
import Primitive
import Prelude (Read, Show, Eq, Bounded, Ord)
-- всевозможные инстансы для классов ниже 

-- Если не страшно, то реализуйте их и для
import List
import Tree

-- Классы
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Monoid a => Group a where
    ginv :: a -> a

-- Инстансы писать сюда
newtype SumNat = SumNat { getSumNat :: Nat } deriving (Read, Show)
newtype SumInt = SumInt { getSumInt :: Int } deriving (Read, Show)
newtype SumRat = SumRat { getSumRat :: Rat } deriving (Read, Show)

newtype ProductNat = ProductNat { getProductNat :: Nat } deriving (Read, Show)
newtype ProductInt = ProductInt { getProductInt :: Int } deriving (Read, Show)
newtype ProductRat = ProductRat { getProductRat :: Rat } deriving (Read, Show)

newtype Any = Any {getAny :: Bool} deriving (Read, Show)
newtype All = All {getAll :: Bool} deriving (Read, Show)

instance Monoid SumNat where 
	mempty = SumNat natZero
	SumNat x `mappend` SumNat y = SumNat $ x +. y  

instance Monoid SumInt where 
	mempty = SumInt intZero
	SumInt x `mappend` SumInt y = SumInt $ x .+. y

instance Monoid SumRat where 
	mempty = SumRat ratZero	
	SumRat x `mappend` SumRat y = SumRat $ x %+ y

instance Monoid ProductNat where 
	mempty = ProductNat natOne
	ProductNat x `mappend` ProductNat y = ProductNat $ x *. y  

instance Monoid ProductInt where 
	mempty = ProductInt intOne
	ProductInt x `mappend` ProductInt y = ProductInt $ x .*. y

instance Monoid Any where
	mempty = Any False
	Any x `mappend` Any y = Any (x || y)

instance Monoid All where 
	mempty = All True
	All x `mappend` All y = All (x && y)
	
instance Monoid ProductRat where 
	mempty = ProductRat ratOne	
	ProductRat x `mappend` ProductRat y = ProductRat $ x %* y

instance Monoid Tri where 
	mempty = EQ
	LT `mappend` _ = LT 
	EQ `mappend` y = y
	GT `mappend` _ = GT 
 
instance (Monoid a) => Monoid (Maybe a) where
	mempty = Nothing
	m `mappend` Nothing = m
	Nothing `mappend` m = m
	Just x `mappend` Just y = Just (x `mappend` y)

instance Monoid (List a) where 
	mempty = Nil
	mappend = (++)

instance Monoid (Tree a) where 
	mempty = Leaf
	mappend = mergeTreeToRight

instance Group SumInt where 
	ginv (SumInt x) = SumInt $ intNeg x 

instance Group SumRat where
	ginv (SumRat x) = SumRat $ ratNeg x 

instance Group ProductRat where 
	ginv (ProductRat x) = ProductRat $ ratInv x 


