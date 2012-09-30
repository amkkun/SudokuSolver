{-# OPTIONS -Wall #-}

import Data.List
import Control.Monad
import Control.Applicative
import Debug.Trace

type Pos = (Int, Int)
type CellPos = (Pos, Pos)

type Value = Int
type Matrix = [[Value]]
type PosVal = [(CellPos, [Value])] -- Map CellPos [Value]
type ValPos = [(Value, [CellPos])] -- Map Value [CellPos]
  
type Sudoku = (Int, Int, Matrix)
              
toValPos :: PosVal -> ValPos
toValPos pv = do
  val <- nub . concat . map snd $ pv
  return (val, map fst $ filter (\(_, vs) -> elem val vs) pv) 

toPosVal :: ValPos -> PosVal
toPosVal vp = do
  pos <- nub . concat . map snd $ vp
  return (pos, map fst $ filter (\(_, ps) -> elem pos ps) vp)


elimVal :: PosVal -> PosVal
elimVal = elimValSub boxsPV . elimValSub colsPV . elimValSub rowsPV

elimValSub :: (PosVal -> [PosVal]) -> PosVal -> PosVal
elimValSub f = concat . map reduce . f


fixed :: [[a]] -> [a]
fixed = concat . filter single

isFixedPV :: (CellPos, [Value]) -> Bool
isFixedPV = single . snd

single :: [a] -> Bool
single = (==) 1 . length

remove :: Eq a => [a] -> [a] -> [a]
remove xs ys
  | single xs = xs
  | otherwise = filter (`notElem` ys) xs

reduce :: PosVal -> PosVal
reduce pv = map (\(pos, vs) -> (pos, remove vs fixedVals)) pv
  where
    fixedVals = concat $ filter isFixedPV pv >>= return . snd 



rowsPV :: PosVal -> [PosVal]
rowsPV = grouping (\(p1, _) (p2, _) -> isSameRow p1 p2)
  
colsPV :: PosVal -> [PosVal]
colsPV = grouping (\(p1, _) (p2, _) -> isSameCol p1 p2)

boxsPV :: PosVal -> [PosVal]
boxsPV = grouping (\(p1, _) (p2, _) -> isSameBox p1 p2)


rowsVP :: ValPos -> [(Value, [[CellPos]])]
rowsVP = map (\(v, ps) -> (v, grouping isSameRow ps)) 

colsVP :: ValPos -> [(Value, [[CellPos]])]
colsVP = map (\(v, ps) -> (v, grouping isSameCol ps)) 

boxsVP :: ValPos -> [(Value, [[CellPos]])]
boxsVP = map (\(v, ps) -> (v, grouping isSameBox ps)) 


grouping :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
grouping _ [] = []
grouping f l@(x:_) = first : grouping f rest
  where
    (first, rest) = partition (f x) l

elimPos :: ValPos -> ValPos
elimPos = elimPosThree . elimPosTwo . boxsVP . elimPosTwo . colsVP . elimPosTwo . rowsVP -- elimPosFour -- .


elimPosTwo :: [(Value, [[CellPos]])] -> ValPos
elimPosTwo vps = map (\(v, pss) -> (v, concat $ map (`remove` fixedPos) pss)) vps
  where
    fixedPos = concat . map (\(_, pss) -> fixed pss) $ vps


elimPosFour :: ValPos -> ValPos
elimPosFour = elimFour boxsVP' . elimFour colsVP' . elimFour rowsVP'
  where
    elimFour f = concatVP . map elimPosFour' . f

concatVP :: [ValPos] -> ValPos
concatVP = map foo . grouping isSameValVP . concat
  where
    foo vp@((v, _):_) = (v, concat [ps | (_, ps) <- vp])

filterVP :: [CellPos] -> ValPos -> ValPos 
filterVP ps = map (\(v, ps') -> (v, filter (`notElem` ps) ps'))
    
    
    
elimPosFour' :: ValPos -> ValPos
elimPosFour' vp = concat fit ++ rest'
  where
    (fit, rest) = partition (\l@((_, ps):_) -> length l == length ps) $ grouping isSamePosVP vp -- :: [ValPos] length l == 2 && 
    ps = concat . concat $ map (map snd) fit -- (trace ("fit:" ++ show fit) fit)
    rest' = filterVP ps $ concat rest
    tracen = 1

isSameValVP :: (Value, [CellPos]) -> (Value, [CellPos]) -> Bool    
isSameValVP (v1, _) (v2, _) = v1 == v2

isSamePosVP :: (Value, [CellPos]) -> (Value, [CellPos]) -> Bool    
isSamePosVP (_, ps1) (_, ps2) = sort ps1 == sort ps2



elimPosE :: ValPos -> ValPos
elimPosE = elimPosThree . 
           concatVP . map elimPosFour' . boxsVPE . elimPosTwoE . boxsVP .
           concatVP . map elimPosFour' . colsVPE . elimPosTwoE . colsVP .
           concatVP . map elimPosFour' . rowsVPE . elimPosTwoE . rowsVP
 
elimPosTwoE :: [(Value, [[CellPos]])] -> [(Value, [[CellPos]])]
elimPosTwoE vps = map (\(v, pss) -> (v, map (`remove` fixedPos) pss)) vps
  where
    fixedPos = concat . map (\(_, pss) -> fixed pss) $ vps

rowsVPE :: [(Value, [[CellPos]])] -> [ValPos]
rowsVPE vp = grouping (\(_, (p1:_)) (_, (p2:_)) -> p1 `isSameRow` p2) bunch
  where
    bunch = [(v, ps) | (v, pss) <- vp, ps <- pss]
colsVPE :: [(Value, [[CellPos]])] -> [ValPos]
colsVPE vp = grouping (\(_, (p1:_)) (_, (p2:_)) -> p1 `isSameCol` p2) bunch
  where
    bunch = [(v, ps) | (v, pss) <- vp, ps <- pss]
boxsVPE :: [(Value, [[CellPos]])] -> [ValPos]
boxsVPE vp = grouping (\(_, (p1:_)) (_, (p2:_)) -> p1 `isSameBox` p2) bunch
  where
    bunch = [(v, ps) | (v, pss) <- vp, ps <- pss]



rowsVP' :: ValPos -> [ValPos]
rowsVP' vp = grouping (\(_, (p1:_)) (_, (p2:_)) -> p1 `isSameRow` p2) bunch
  where
    bunch = [(v, ps) | (v, pss) <- rowsVP vp, ps <- pss]
                     
colsVP' :: ValPos -> [ValPos]
colsVP' vp = grouping (\(_, (p1:_)) (_, (p2:_)) -> p1 `isSameCol` p2) bunch
  where
    bunch = do
      (v, pss) <- colsVP vp
      ps <- pss
      return (v, ps)

boxsVP' :: ValPos -> [ValPos]
boxsVP' vp = grouping (\(_, (p1:_)) (_, (p2:_)) -> p1 `isSameBox` p2) bunch
  where
    bunch = do
      (v, pss) <- boxsVP vp
      ps <- pss
      return (v, ps)


elimPosThree :: ValPos -> ValPos
elimPosThree = map (\(v, ps) -> (v, (removeLine isSameCol . removeLine isSameRow) ps))
  
removeLine :: (CellPos -> CellPos -> Bool) -> [CellPos] -> [CellPos]  
removeLine f ps = filter2 cond ps fit 
  where
    fit = concat . map (map head) . filter single . map (grouping f) . grouping isSameBox $ ps -- concat $ filter single (grouping isSameBox ps >>= return . grouping f) >>= return . map head
    cond p1 p2 = isSameBox p1 p2 || not (f p1 p2)




filter2 :: (a -> b -> Bool) -> [a] -> [b] -> [a]
filter2 _ [] _ = []
filter2 f (x:xs) ys 
  | all (f x) ys = x : filter2 f xs ys  
  | otherwise = filter2 f xs ys


eliminate :: PosVal -> PosVal
eliminate  pv = -- if pv == pv' then pv else eliminate pv' 
  -- where
  --   pv' = 
      toPosVal . elimPos . toValPos . elimVal $ pv


isSameRow :: CellPos -> CellPos -> Bool
isSameRow p1 p2 = fst (fst p1) == fst (fst p2) &&
                  fst (snd p1) == fst (snd p2)

isSameCol :: CellPos -> CellPos -> Bool
isSameCol p1 p2 = snd (fst p1) == snd (fst p2) &&
                  snd (snd p1) == snd (snd p2)

isSameBox :: CellPos -> CellPos -> Bool
isSameBox p1 p2 = fst p1 == fst p2



mainSolve :: PosVal -> [PosVal]
mainSolve pv
  | isFailed pv = []
  | isCompleted pv = return pv
  | otherwise = update pv >>= mainSolve . eliminate 
                
isCompleted :: PosVal -> Bool                
isCompleted = all (\(_, vs) -> single vs)

isFailed :: PosVal -> Bool
isFailed pv = valErr pv || lenErr pv || any (\(_, vs) -> null vs) pv

lenErr :: PosVal -> Bool
lenErr pv = length pv /= boardsize * boardsize 
  where
    boardsize = maximum . concat . map snd $ pv

valErr :: PosVal -> Bool
valErr pv = or (map isDup (map takeSingle (rowsPV pv))) ||
            or (map isDup (map takeSingle (colsPV pv))) ||
            or (map isDup (map takeSingle (boxsPV pv)))

takeSingle :: [(a, [b])] -> [[b]]
takeSingle = filter single . map snd

isDup :: Eq a => [a] -> Bool
isDup [] = False
isDup (x:xs) = elem x xs || isDup xs

update :: PosVal -> [PosVal]
update pv = [pv1 ++ (minpv:pv2) | minpv <- minpvs] 
  where
    comp (_, vs1) (_, vs2) = compare (length vs1) (length vs2)
    minchoice = minimumBy comp (filter (\(_, vs) -> length vs > 1) pv)
    (pv1, _:pv2) = break (\(p, _) -> p == fst minchoice) pv
    minpvs = snd minchoice >>= \x -> [(fst minchoice, [x])]
    
solve :: Sudoku -> [Matrix]
solve = map toMatrix . mainSolve . eliminate . analyze

analyze :: Sudoku -> PosVal
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

toMatrix :: PosVal -> Matrix
toMatrix pv = divide boardsize . concat . map snd $ sorted
  where
    boardsize = intSqrt (length pv)
    sorted = sortBy (\(p1, _) (p2, _) -> compare (replace p1) (replace p2)) pv
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

display :: Matrix -> IO ()
display = mapM_ (putStrLn . concat . intersperse " " . map show)

getSudoku :: IO Sudoku
getSudoku = do
  (rowsize:colsize:_) <- map read . words <$> getLine
  let boardsize = rowsize * colsize
  matrix <- forM [1..boardsize] (\_ -> map read . words <$> getLine)
  return (rowsize, colsize, matrix)
  
main :: IO ()
main = do
  -- files <- getArgs
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
  mapM_ display $ solve (3, 3, sudoku)
  -- mapM_ (\_ -> putStr "##") $ solve (3, 3, sudoku)
  putStrLn "--"
  
showSolve :: Sudoku -> IO ()
showSolve = mapM_ display . solve 

easy :: Sudoku
easy = (3, 3, [ [2,5,0,0,3,0,0,4,6]
              , [0,0,9,0,2,0,0,0,8]
              , [4,3,0,7,6,1,0,0,9]
              , [0,0,0,6,0,0,0,0,0]
              , [1,0,0,9,8,4,0,0,5]
              , [0,0,0,0,0,2,0,0,0]
              , [3,0,0,1,4,8,0,7,2]
              , [8,0,0,0,7,0,9,0,0]
              , [7,4,0,0,9,0,0,5,3]
              ])


small :: Sudoku
small = (2, 2, [ [1,2,0,4]
              , [0,0,0,2]
              , [0,3,4,1]
              , [4,1,2,3]
              ])
              
              -- [ [1,2,3,4]
              -- , [3,4,1,2]
              -- , [2,3,4,1]
              -- , [4,1,2,3]
              -- ]

hard :: Sudoku
hard = (3, 3, [ [8,0,0,0,0,0,0,0,0]
              , [0,0,3,6,0,0,0,0,0]
              , [0,7,0,0,9,0,2,0,0]
              , [0,5,0,0,0,7,0,0,0]
              , [0,0,0,0,4,5,7,0,0]
              , [0,0,0,1,0,0,0,3,0]
              , [0,0,1,0,0,0,0,6,8]
              , [0,0,8,5,0,0,0,1,0]
              , [0,9,0,0,0,0,4,0,0]
              ])
       
large :: Sudoku
large = (5, 5, [ [0,12,0,10,0,13,0,7,0,11,0,0,8,0,0,23,0,21,0,14,0,15,0,9,0]
               , [0,22,11,25,0,1,0,15,0,18,0,0,6,0,0,8,0,12,0,4,0,5,21,17,0]
               , [1,0,0,0,8,0,0,10,0,0,24,7,0,4,25,0,0,5,0,0,11,0,0,0,2]
               , [18,0,7,0,6,0,22,0,2,0,0,5,0,9,0,0,13,0,16,0,20,0,10,0,24]
               , [0,4,0,23,0,5,0,24,0,3,0,0,21,0,0,11,0,22,0,25,0,13,0,7,0]
               , [19,0,0,0,1,0,14,11,24,0,15,0,0,0,16,0,8,9,13,0,4,0,0,0,6]
               , [0,0,13,0,0,15,0,12,0,4,25,0,10,0,23,5,0,24,0,2,0,0,7,0,0]
               , [11,23,0,9,20,21,5,0,18,22,0,14,0,7,0,15,19,0,25,6,17,12,0,3,8]
               , [15,0,18,0,21,0,0,19,0,0,5,0,11,0,22,0,0,4,0,0,24,0,20,0,10]
               , [0,25,0,24,17,0,10,0,3,0,21,0,0,0,6,0,20,0,11,0,19,1,0,5,0]
               , [10,0,0,0,11,9,17,0,4,0,0,21,0,6,0,0,12,0,22,16,18,0,0,0,23]
               , [0,14,0,18,0,10,0,1,0,5,17,0,20,0,3,9,0,25,0,23,0,11,0,6,0]
               , [13,0,0,0,3,0,0,16,0,0,10,1,0,2,14,0,0,18,24,0,8,0,0,0,22]
               , [0,5,12,6,0,11,0,18,0,8,0,0,16,0,0,10,0,2,0,19,0,3,14,20,0]
               , [16,0,0,0,25,0,19,0,23,0,11,0,4,0,18,0,14,0,20,0,5,0,0,0,1]
               , [0,21,1,14,0,24,0,0,0,17,0,10,9,5,0,6,0,0,0,13,0,19,18,2,0]
               , [12,0,0,15,18,0,4,6,1,0,16,0,2,0,21,0,25,11,5,0,7,22,0,0,13]
               , [0,10,19,17,0,16,0,0,0,12,7,18,0,1,13,2,0,0,0,15,0,24,11,21,0]
               , [25,0,0,0,22,7,0,5,0,10,0,23,0,20,0,18,0,1,0,17,12,0,0,0,15]
               , [0,24,0,16,0,0,3,0,8,0,4,0,22,0,15,0,7,0,12,0,0,17,0,1,0]
               , [4,0,0,0,15,3,0,21,0,14,0,11,19,8,0,22,0,13,0,24,2,0,0,0,7]
               , [8,19,0,20,10,0,16,0,5,0,23,0,0,0,9,0,2,0,14,0,22,18,0,11,3]
               , [23,0,0,11,12,0,18,8,10,0,2,25,0,24,20,0,9,7,6,0,13,14,0,0,21]
               , [0,7,22,3,0,2,25,0,11,24,0,0,15,0,0,4,18,0,19,20,0,10,1,8,0]
               , [0,18,0,2,0,23,0,4,0,19,0,22,14,12,0,16,0,8,0,1,0,20,0,15,0]
               ])
        
-- extra :: Sudoku
-- extra = (6, 5, [])
-- 0,0,28,16,0,29,0,8,14,15,0,0,0,2,0,13,7,5,0,0,0,0,0,27,0,20,0,25,0]
-- 0,0,24,0,12,0,27,28,0,4,0,17,0,6,0,0,0,11,0,23,0,3,0,1,0,2,0,30,0,0]
-- 10,18,30,17,0,25,0,0,7,15,0,0,0,11,0,3,0,26,0,4,13,0,8,23,0,0,21,0,27,0]
-- 0,0,0,26,15,0,17,18,0,20,0,1,0,5,0,9,8,0,22,27,25,0,16,12,0,0,19,0,29]
-- 27,11,9,0,3,0,13,23,21,0,0,16,0,10,0,2,0,18,0,20,0,29,0,28,0,12,0,14,0,26]
-- 0,2,6,25,0,9,0,0,1,0,8,0,0,0,27,0,30,12,10,0,0,19,11,0,5,13,23,0,28,0]
-- 0,0,0,0,16,0,25,10,15,0,9,19,0,29,0,4,0,0,0,18,17,6,0,22,3,0,0,27,5,28]
-- 0,21,12,9,0,0,0,30,0,0,10,25,15,0,24,13,17,0,20,6,0,14,0,27,0,0,0,3,4,0]
-- 0,0,0,6,0,11,9,0,24,8,0,30,16,0,0,0,3,0,26,0,19,0,5,0,1,15,7,21,0,20]
-- 0,3,15,14,0,23,18,17,0,6,0,26,27,0,0,30,0,22,0,21,2,0,12,0,9,19,11,0,13,8]

         

failed :: Sudoku
failed = (2, 2, [ [1, 2, 3, 4]
                , [2, 3, 0, 0]
                , [0, 0, 0, 0]
                , [0, 0, 0, 0]
                ])
