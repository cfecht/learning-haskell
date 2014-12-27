module Lookup
where

myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((k,v) : r) = 
    if key == k
       then Just v
       else myLookup key r


