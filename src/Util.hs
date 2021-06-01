module Util where

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes = (foldr (.) id.) . replicate

pointAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
pointAdd (y, x) (dy, dx) = (y + dy, x + dx)

within :: (Int, Int) -> Int -> Bool
within (low, hi) n = n >= low &&
                     n <= hi

withinSquare :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
withinSquare (lowy, lowx) (hiy, hix) (y, x) = within (lowy, hiy) y &&
                                              within (lowx, hix) x

------------------------------------------------------------
-- Functions for utilizing arrays.
------------------------------------------------------------

-- Take the matrix from origin to corner.
sliceBox :: (Int, Int) -> (Int, Int) -> [[a]] -> [[a]]
sliceBox (y0, x0) (y, x) matrix = map (slice x0 x) (slice y0 y matrix)

slice :: Int -> Int -> [a] -> [a]
slice from to list = take (to + 1 - (max from 0)) (drop from list)

at :: Int -> [a] -> a
at 0 (x:_)  = x
at n (_:xs) = at (n - 1) xs
at _ []     = error "Out of bounds!"

atPut :: Int -> a -> [a] -> [a]
atPut 0 x' (_:xs) = x' : xs
atPut n x' (x:xs) = x  : (atPut (n - 1) x' xs)
atPut _ _  []     = error "Out of bounds!"

atyx :: (Int, Int) -> [[a]] -> a
atyx (y, x) lst = at x (at y lst)

atyxPut :: (Int, Int) -> a -> [[a]] -> [[a]]
atyxPut (y, x) elt lst = atPut y (atPut x elt (at y lst)) lst
