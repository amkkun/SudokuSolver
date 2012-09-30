{-# OPTIONS -Wall #-}

import Data.List
import Control.Monad
import Control.Applicative

type Pos = (Int, Int)
type CellPos = (Pos, Pos)

type Val = Int
type Matrix a = [[a]]
type Sudoku = (Int, Int, Matrix Val)

type ListMap k a = [(k, [a])]

type PosMap = ListMap CellPos Val
type ValMap = ListMap Val CellPos
  
-- | solve main loop
solve :: PosMap -> [PosMap]
solve pm
  | isFailed pm = []
  | isCompleted pm = return pm
  | otherwise = update pm >>= solve . eliminate

isFailed :: PosMap -> Bool
isFailed pm = hasEmpty pm || hasDup pm

hasEmpty :: PosMap -> Bool
hasEmpty = any (null . snd)

hasDup :: PosMap -> Bool
hasDup pm = or $ do
  (pos, vals) <- determinate
  let related = filter (isRelatedPos pos . fst) determinate
      relatedVals = concatMap snd related
  return $ any (`elem` relatedVals) vals
  where
    determinate = filter (isSingle . snd) pm

isCompleted :: PosMap -> Bool
isCompleted = all (isSingle . snd)

update :: PosMap -> [PosMap]
update pm = [pv1 ++ (minpv:pv2) | minpv <- minpvs] 
  where
    comp (_, vs1) (_, vs2) = compare (length vs1) (length vs2)
    minchoice = minimumBy comp (filter (\(_, vs) -> length vs > 1) pm)
    (pv1, _:pv2) = break (\(p, _) -> p == fst minchoice) pm
    minpvs = snd minchoice >>= \x -> [(fst minchoice, [x])]


-- | eliminate main
eliminate :: PosMap -> PosMap
eliminate pm
  | pm == pm' = pm'
  | otherwise = eliminate pm'
  where
    pm' = toPosMap . elimLine . elimPos . toValMap . elimDet $ pm

-- | PosMap <-> ValMap

toValMap :: PosMap -> ValMap
toValMap m = do
  a <- sort . nub . concat . map snd $ m
  return (a, map fst $ filter (\(_, ks) -> elem a ks) m) 

toPosMap :: ValMap -> PosMap
toPosMap m = do
  a <- restorePos . nub . concatMap snd $ m
  return (a, map fst $ filter (\(_, ks) -> elem a ks) m)

restorePos :: [CellPos] -> [CellPos]
restorePos ps = sort [(bp, p) | bp <- [(a, b) | a <- [1..fsts], b <- [1..snds]], p <- [(a, b) | a <- [1..snds], b <- [1..fsts]]]
  where
    fsts = maximum $ fst . fst <$> ps
    snds = maximum $ snd . fst <$> ps
  
      
-- | eliminate determinate value
elimDet :: PosMap -> PosMap
elimDet = op isSameCol . op isSameRow . op isSameBox
  where
    op f = concatMap reduceDet . subGroupPM f
    
subGroupPM :: (CellPos -> CellPos -> Bool) -> PosMap -> [PosMap]
subGroupPM f = grouping (\(pos1, _) (pos2, _) -> f pos1 pos2)

reduceDet :: PosMap -> PosMap
reduceDet pm = map (\(p, vs) -> (p, if isSingle vs
                                    then vs
                                    else filterDup detVals vs)) pm
  where
    detVals = concat $ filter isSingle $ map snd pm

-- | eliminate position
elimPos :: ValMap -> ValMap
elimPos = op isSameRow . op isSameCol . op isSameBox
  where
    op f = concatVM . map elimPosEx' . subGroupVM f

-- elimPos' :: ValMap -> ValMap
-- elimPos' vm = map (\(v, pss) -> (v, if isSingle pss
--                                     then pss
--                                     else filterDup fixedPos pss)) vm
--   where
--     fixedPos = concat . filter isSingle . map snd $ vm

elimPosEx' :: ValMap -> ValMap
elimPosEx' vm = join fit ++ rest'
  where
    (fit, rest) = partition (\s -> length s == length (snd $ head s)) $ grouping (\(_, s1) (_, s2) -> s1 == s2) vm 
    fitPos = join . join $ liftM (liftM snd) fit
    rest' = fmap (\(v, ps) -> (v, filter (not . (`elem` fitPos)) ps)) $ join rest                                     

concatVM :: [ValMap] -> ValMap
concatVM vms = map concatPos $ grouping (\vp vp' -> fst vp == fst vp') $ concat vms
  where
    concatPos vm = (fst $ head vm, concatMap snd vm)

subGroupVM :: (CellPos -> CellPos -> Bool) -> ValMap -> [ValMap]
subGroupVM isSame vm = grouping samePos $ do
  (val, poss) <- vm
  poss' <- grouping isSame poss
  return (val, poss')
  where
    samePos (_, ps) (_, ps') = isSame (head ps) (head ps')

-- | eliminate line
elimLine :: ValMap -> ValMap
elimLine = map (\(v, ps) -> (v, (reduceLine isSameCol . reduceLine isSameRow) ps))

reduceLine :: (CellPos -> CellPos -> Bool) -> [CellPos] -> [CellPos]
reduceLine f ps = filter2 cond ps fit
  where
    fit = concatMap (map head) . filter isSingle . map (grouping f) . grouping isSameBox $ ps
    cond p1 p2 = isSameBox p1 p2 || not (f p1 p2)

-- | lib
isSingle :: [a] -> Bool
isSingle = (== 1) . length

filterDup :: Eq a => [a] -> [a] -> [a]
filterDup xs = filter (not . (`elem` xs))

grouping :: (a -> a -> Bool) -> [a] -> [[a]]
grouping f xs
  | null xs = []
  | otherwise = fit : grouping f rest
  where
    (fit, rest) = partition (f $ head xs) xs

filter2 :: (a -> b -> Bool) -> [a] -> [b] -> [a]
filter2 _ [] _ = []
filter2 f (x:xs) ys
  | all (f x) ys = x : filter2 f xs ys
  | otherwise = filter2 f xs ys
  
-- | position

isRelatedPos :: CellPos -> CellPos -> Bool
isRelatedPos a b = a /= b && (isSameBox a b || isSameRow a b || isSameCol a b)

isSameRow :: CellPos -> CellPos -> Bool
isSameRow p1 p2 = fst (fst p1) == fst (fst p2) &&
                  fst (snd p1) == fst (snd p2)

isSameCol :: CellPos -> CellPos -> Bool
isSameCol p1 p2 = snd (fst p1) == snd (fst p2) &&
                  snd (snd p1) == snd (snd p2)

isSameBox :: CellPos -> CellPos -> Bool
isSameBox p1 p2 = fst p1 == fst p2




-- | IO




    
runSolve :: Sudoku -> [Matrix Val]
runSolve = map toMatrix . solve . eliminate . analyze

showSolve :: Sudoku -> IO ()
showSolve = mapM_ display . runSolve 


analyze :: Sudoku -> PosMap
analyze (row, col, matrix) = 
  zip (cellposList row col) (vals matrix)
  where
    vals = map (\x -> if x == 0 then [1..(row * col)] else [x]) . concat

cellposList :: Int -> Int -> [CellPos]
cellposList rsize csize = cps (1,1) (1,1) 
  where
    cps (br, bc) (r, c)
      | br > csize = []
      | r > rsize = cps (br + 1, 1) (1, 1)
      | bc > rsize = cps (br, 1) (r + 1, 1)
      | c > csize = cps (br, bc + 1) (r, 1)
      | otherwise = ((br, bc), (r, c)) : cps (br, bc) (r, c + 1)

toMatrix :: PosMap -> Matrix Val
toMatrix pm = divide boardsize . concat . map snd $ sorted
  where
    boardsize = intSqrt (length pm)
    sorted = sortBy (\(p1, _) (p2, _) -> compare (replace p1) (replace p2)) pm
    replace ((a, b), (c, d)) = ((a, c), (b, d))

intSqrt :: Int -> Int
intSqrt num = intSqrt' num 1
  where
    intSqrt' n m
      | n < m * m = m - 1
      | otherwise = intSqrt' n (m + 1)

divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = take n xs : divide n (drop n xs)

display :: Matrix Val -> IO ()
display = mapM_ (putStrLn . unwords . map show)

getSudoku :: IO Sudoku
getSudoku = do
  (rowsize:colsize:_) <- map read . words <$> getLine
  let boardsize = rowsize * colsize
  matrix <- forM [1..boardsize] (\_ -> map read . words <$> getLine)
  return (rowsize, colsize, matrix)
  
main :: IO ()
main = do
  -- filepath <- getArgs
  sudoku <- getSudoku
  showSolve sudoku
  -- mapM_ display answers
  -- putStrLn "--"
  -- showSolve hard
  -- sudoku17
  --showSolve large
  
sudoku17 :: IO ()
sudoku17 = forever $ do
  sudoku <- divide 9 . map read . divide 1 <$> getLine
  showSolve (3, 3, sudoku)
  -- mapM_ (\_ -> putStr "##") $ solve (3, 3, sudoku)
  putStrLn "--"
  
