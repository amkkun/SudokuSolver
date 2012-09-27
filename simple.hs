{-# OPTIONS -Wall #-}

import Data.List
import Control.Applicative
import Control.Monad

type Matrix a = [[a]]

main :: IO ()
main = do
  sudoku <- forM [1..9] (\_ -> map read . words <$> getLine) 
  let answers = solve sudoku
  mapM_ (mapM_ (putStrLn . unwords . map show)) answers

solve :: Matrix Int -> [Matrix Int]
solve matrix 
  | isFailed matrix = []
  | isCompleted matrix = return matrix
  | otherwise = update matrix >>= solve
                
isFailed :: Matrix Int -> Bool                
isFailed matrix = anyDup [matrix, transpose matrix, box matrix]
  where
    anyDup = any (any (duplicate . filter (/= 0)))
    box = map concat . concat . map (divide 3) . transpose . map (divide 3)
  
isCompleted :: Matrix Int -> Bool
isCompleted = all (all (/= 0))
  
update :: Matrix Int -> [Matrix Int]
update matrix =
  [left ++ [left' ++ [val] ++ right'] ++ right | val <- [1..9]]
  where
    (left, fit:right) = break (any (== 0)) matrix
    (left', _:right') = break (== 0) fit
    
divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = left : divide n right    
  where
    (left, right) = splitAt n xs
    
duplicate :: Eq a => [a] -> Bool
duplicate [] = False
duplicate (x:xs) = elem x xs || duplicate xs     
    
