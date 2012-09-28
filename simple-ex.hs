{-# OPTIONS -Wall #-}

import Data.List
import Control.Applicative
import System.Environment

type Matrix a = [[a]]

main :: IO ()
main = do
  (filepath:_) <- getArgs
  strs <- lines <$> readFile filepath  
  normal strs
  sudoku17 strs

normal :: [String] -> IO ()
normal strs = do
  let (rsize:csize:_) = map read . words $ head strs
      nums = concat $ map read . words <$> tail strs
      answers = map row $ solve $ analyze rsize csize nums
  mapM_ (mapM_ (putStrLn . unwords . map show)) answers

sudoku17 :: [String] -> IO ()
sudoku17 = mapM_ sudoku17'
  where
    sudoku17' str = do
      let sudoku = analyze 3 3 . map read $ divide 1 str
      mapM_ (mapM_ (putStrLn . unwords . map show)) $ map row $ solve sudoku 
      putStrLn "--"

analyze :: Int -> Int -> [Int] -> Matrix (Matrix Int)
analyze rsize csize = map transpose . divide csize . divide rsize . divide csize

solve :: Matrix (Matrix Int) -> [Matrix (Matrix Int)]
solve matrix 
  | isFailed matrix = []
  | isCompleted matrix = return matrix
  | otherwise = update matrix >>= solve
                
isFailed :: Matrix (Matrix Int) -> Bool                
isFailed matrix = anyDup [row matrix, col matrix, box matrix]
  where
    anyDup = any (any (duplicate . filter (/= 0)))
    
row, col, box :: Matrix (Matrix Int) -> Matrix Int
row = box . map transpose
col = transpose . row
box = map concat . concat

size :: Matrix (Matrix a) -> Int
size matrix = (length $ head matrix) * (length matrix)

isCompleted :: Matrix (Matrix Int) -> Bool
isCompleted = all (all (all (all (/= 0))))
  
update :: Matrix (Matrix Int) -> [Matrix (Matrix Int)]
update matrix =
  [left ++ [left' ++ [left'' ++ [left''' ++ [val] ++ right'''] ++ right''] ++ right'] ++ right | val <- [1..(size matrix)]]
  where
    (left, fit:right) = break (any (any (any (== 0)))) matrix
    (left', fit':right') = break (any (any (== 0))) fit
    (left'', fit'':right'') = break (any (== 0)) fit'
    (left''', _:right''') = break (== 0) fit''
    
divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = left : divide n right    
  where
    (left, right) = splitAt n xs
    
duplicate :: Eq a => [a] -> Bool
duplicate [] = False
duplicate (x:xs) = elem x xs || duplicate xs     
