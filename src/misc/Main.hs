module Main where

import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (guard)
import Data.Functor ((<$>))
import Data.Monoid
import Control.Monad.Writer

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x : xs) = (:) <$> x <*> sequenceA xs

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole  = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right) 
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing
    
banana :: Pole -> Maybe Pole
banana _ = Nothing

marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)
    
routine :: Maybe Pole
routine = do 
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second
    

(-:) :: a -> (a ->b) -> b
x -: f = f x

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1,2]
  ch <- ['a', 'b']
  return (n, ch)
  
listOfTuples' :: [(Int, Char)]
listOfTuples' = ([1,2]) >>= (\n -> (['a', 'b']) >>= (\ch -> return (n, ch)))

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x
    
sevensOnly' :: [Int]
sevensOnly' = [1..50] >>= (\x -> guard ('7' `elem` show x) >>= (\_ -> return x))

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')  

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem`in3 start

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: Monoid m => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int 

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

newtype MyWriter w a = MyWriter { runMyWriter :: (a, w) }

instance (Monoid w) => Monad (MyWriter w) where
    return x = MyWriter (x, mempty)
    (MyWriter (x,v)) >>= f = let MyWriter (y, v') = f x in MyWriter (y, v `mappend` v')

logNumber :: Int -> MyWriter [String] Int
logNumber x = MyWriter (x, ["Got number: " ++ show x])

multiWithLog :: MyWriter [String] Int
multiWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a * b)
    
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b 
    | b == 0 = do 
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod `b)]
        gcd' b (a `mod` b)

