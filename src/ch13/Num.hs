module Num where

import Data.List (intercalate)

-----------------------------------------------------------------
-- Symbolic/units manipulation
-----------------------------------------------------------------

-- The "operators" that we are going to support
data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq, Show)
        
{- The core symbolic manipulation type.  It can be a simple number,
a symbol, a binary arithmetic operation (such as +), or a unary
arithmetic operation (such as cos)

Notice the types of BinaryArith and UnaryArith: it's a recursive
type.  So, we could represent a (+) over two SymbolicManips. -}
data SymbolicManip a = 
           Number a 
         | Symbol String
         | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
         | UnaryArith String (SymbolicManip a)
           deriving (Eq)
           
instance Num a => Num(SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b
    a * b = BinaryArith Mul a b
    negate a = BinaryArith Mul (Number (-1)) a
    abs a = UnaryArith "abs" a
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)
    
instance Fractional a => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)
    
instance Floating a => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp a = UnaryArith "exp" a
    log a = UnaryArith "log" a
    sqrt a = UnaryArith "sqrt" a
    a ** b = BinaryArith Pow a b
    sin a = UnaryArith "sin" a
    cos a = UnaryArith "cos" a
    tan a = UnaryArith "tan" a
    asin a = UnaryArith "asin" a
    acos a = UnaryArith "acos" a
    atan a = UnaryArith "atan" a
    sinh a = UnaryArith "sinh" a
    cosh a = UnaryArith "cosh" a
    tanh a = UnaryArith "tanh" a
    asinh a = UnaryArith "asinh" a
    acosh a = UnaryArith "acosh" a
    atanh a = UnaryArith "atanh" a
    
prettyShow :: (Show a, Num a) => SymbolicManip a -> String
 
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
 
prettyShow (BinaryArith op a b) = 
    let pa = simpleParen a
        pb = simpleParen b
        pop = op2str op
        in pa ++ pop ++ pb
 
prettyShow (UnaryArith opstr a) = 
     opstr ++ "(" ++ prettyShow a ++ ")"
      
op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _) = prettyShow x

{- Showing a SymbolicManip calls the prettyShow function on it -}
instance (Show a, Num a) => Show (SymbolicManip a) where
    show a = prettyShow a


simplify :: (Eq a, Num a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op ia ib) =
    let sa = simplify ia
        sb = simplify ib
    in case (op, sa, sb) of 
               (Mul, Number 1, b) -> b
               (Mul, a, Number 1) -> a
               (Mul, a, Number 0) -> 0
               (Mul, Number 0, b) -> 0
               (Plus, a, Number 0) -> a
               (Plus, Number 0, b) -> b
               (Minus, a, Number 0) -> a
               _ -> BinaryArith op sa sb
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x
rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow i = 
    let toList (Number x) = [show x]
        toList (Symbol x) = []
        toList (BinaryArith op a b) = toList a ++ toList b ++ [op2str op]
        toList (UnaryArith op a) = toList a ++ [op]
    in intercalate " " (toList i)
     
test :: Num a => a
test = 3*5 + 7

data Units a = Units a (SymbolicManip a)
               deriving (Eq)
           
instance (Eq a, Num a) => Num (Units a) where
    (Units xa ua) + (Units xb ub)
        | ua == ub = Units (xa + xb) ua
        | otherwise = error "Mis-matched units in add or subtract"
    (Units xa ua) - (Units xb ub) = (Units xa ua) + (Units (xb * (-1)) ub)
    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa _) = Units (signum xa) (Number 1)
    fromInteger i = Units (fromInteger i) (Number 1)