module Main where

class Tofu t where
   tofu :: j a -> t a j
   
data Frank a b = Frank {frankField :: b a} deriving (Show)

class MyFunctor f where
   myfmap :: (a -> b) -> f a -> f b
   
data MyMaybe a = MyNothing | MyJust a deriving (Show)

instance MyFunctor MyMaybe where
   myfmap _ MyNothing  = MyNothing
   myfmap f (MyJust x) = MyJust (f x)
   
data CMaybe a = CNothing | CJust Int a deriving (Show)




