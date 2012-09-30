import Data.Sequence (Seq, (<|), (><))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List



type Matrix a = [[a]]
type Sudoku = (Int, Int, Matrix Val)

type SeqMap k a = Seq (k, Seq a)

type Choices = Seq Val

type Val = Int
type Pos = (Int, Int)
type CellPos = (Pos, Pos)

type PosMap = SeqMap CellPos Val
type ValMap = SeqMap Val CellPos

-- data Hatena a = Def a | Yet (Seq a) 

-- * solve main loop
solve :: PosMap -> Seq PosMap
solve pm
  | isFailed pm = S.empty
  | isCompleted pm = return pm
  | otherwise = update pm >>= solve . eliminate

isFailed :: PosMap -> Bool
isFailed pm = hasEmpty pm || hasDup pm

hasEmpty :: PosMap -> Bool
hasEmpty = F.any (S.null . snd)

hasDup :: PosMap -> Bool
hasDup pm = F.or $ do
  (pos, vals) <- determinate
  let related = S.filter (isRelatedPos pos . fst) determinate
      relatedVals = join $ liftM snd related
  return $ F.any (`F.elem` relatedVals) vals 
  where
    determinate = S.filter (isSingle . snd) pm

isCompleted :: PosMap -> Bool
isCompleted = F.all (isSingle . snd)

update :: PosMap -> Seq PosMap
update pm = do
  val <- vals
  return $ S.update minIndex (pos, S.singleton val) pm
  where
    notSingle = S.filter (not . isSingle . snd) pm
    (pos, vals) = F.minimumBy compareSeqLen notSingle
    minIndex = fromJust $ S.elemIndexL (pos, vals) pm

compareSeqLen :: (a, Seq b) -> (c, Seq d) -> Ordering
compareSeqLen a b = compare (S.length $ snd a) (S.length $ snd b)

-- * eliminate main
eliminate :: PosMap -> PosMap
eliminate pm
  | pm == pm' = pm'
  | otherwise = eliminate pm'
  where
    pm' = toPosMap . elimLine . elimPos . toValMap . elimDeterminate $ pm

-- | PosMap <-> ValMap
toValMap :: PosMap -> ValMap
toValMap pm = do
  val <- S.sort . seqnub . join . liftM snd $ pm
  return (val, fmap fst $ S.filter (\(_, vals) -> F.elem val vals) pm)
    
toPosMap :: ValMap -> PosMap
toPosMap vm = do
  pos <- restorePos . seqnub . join $ snd <$> vm 
  return (pos, fmap fst $ S.filter (\(_, poss) -> F.elem pos poss) vm)

seqnub :: Eq a => Seq a -> Seq a
seqnub xs 
  | S.null xs = S.empty
  | otherwise = first <| S.filter (/= first) xs
  where
    first = S.index xs 0

restorePos :: Seq CellPos -> Seq CellPos
restorePos ps = S.sort $ do
  bp <- do
    a <- S.fromList [1..fsts]
    b <- S.fromList [1..snds]
    return (a, b)
  p <- do
    a <- S.fromList [1..snds]
    b <- S.fromList [1..fsts]
    return (a, b)
  return (bp, p)
  where
    fsts = F.maximum $ fst . fst <$> ps
    snds = F.maximum $ snd . fst <$> ps
    
-- * eliminate determinate value
elimDeterminate :: PosMap -> PosMap
elimDeterminate pm = pm >>= return . elimDeterminate' determinate
  where
    determinate = S.filter (isSingle . snd) pm

elimDeterminate' :: PosMap -> (CellPos, Seq Val) -> (CellPos, Seq Val)
elimDeterminate' pm pair@(pos, vals)
  | isSingle $ snd pair = pair
  | otherwise = (pos, filterDup relatedVals vals)
  where
    relatedPM = S.filter (isRelatedPos pos . fst) pm 
    relatedVals = join $ liftM snd relatedPM


-- * eliminate position 
elimPos :: ValMap -> ValMap
elimPos = op isSameRow . op isSameCol . op isSameBox
  where
    op f = joinVM . liftM elimPosEx' . subGroup f -- elimPos' <-> elimPosEx'

-- only one
elimPos' :: ValMap -> ValMap
elimPos' vm = fmap (\(v, pss) -> (v, if isSingle pss
                                     then pss
                                     else filterDup fixedPos pss)) vm
  where
    fixedPos = join . S.filter isSingle . fmap snd $ vm 

-- extension
elimPosEx' :: ValMap -> ValMap
elimPosEx' vm = join fit >< rest'
  where
    (fit, rest) = S.partition (\s -> S.length s == S.length (snd $ S.index s 0)) $ grouping (\(_, s1) (_, s2) -> s1 == s2) vm 
    fitPos = join . join $ liftM (liftM snd) fit
    rest' = fmap (\(v, ps) -> (v, S.filter (not . (`F.elem` fitPos)) ps)) $ join rest
    
joinVM :: Seq ValMap -> ValMap
joinVM svm = liftM joinPos $ grouping (\vp vp' -> fst vp == fst vp') $ join svm
  where
    joinPos vm = (fst $ S.index vm 0, join $ liftM snd vm)
  
subGroup :: (CellPos -> CellPos -> Bool) -> ValMap -> Seq ValMap
subGroup isSame vm = grouping samePos tmp 
  where
    tmp = do
      (val, poss) <- vm
      poss' <- grouping isSame poss
      return (val, poss')
    samePos (v, ps) (v', ps') = isSame (S.index ps 0) (S.index ps' 0) 

-- * eliminate Line
elimLine :: ValMap -> ValMap
elimLine = fmap (\(v, ps) -> (v, (reduceLine isSameCol . reduceLine isSameRow) ps))

reduceLine :: (CellPos -> CellPos -> Bool) -> Seq CellPos -> Seq CellPos
reduceLine f ps = filter2 cond ps fit
  where
    fit = join . liftM (liftM (`S.index` 0)) . S.filter isSingle . liftM (grouping f) . grouping isSameBox $ ps
    cond p1 p2 = isSameBox p1 p2 || not (f p1 p2)


-- * lib
isSingle :: Seq a -> Bool
isSingle = (== 1) . S.length

-- ys から xs の要素を引く
filterDup :: Eq a => Seq a -> Seq a -> Seq a
filterDup xs = S.filter (not . (`F.elem` xs))

grouping :: (a -> a -> Bool) -> Seq a -> Seq (Seq a)
grouping f xs
  | S.null xs = S.empty
  | otherwise = fit <| grouping f rest
  where
    (fit, rest) = S.partition (f $ S.index xs 0) xs

filter2 :: (a -> b -> Bool) -> Seq a -> Seq b -> Seq a
filter2 f xs ys
  | S.null xs = S.empty
  | F.all (f x) ys = x <| filter2 f (S.drop 1 xs) ys
  | otherwise = filter2 f (S.drop 1 xs) ys
  where
    x = S.index xs 0

    
-- * position
takeRelatedPos :: CellPos -> Seq CellPos -> Seq CellPos
takeRelatedPos p = S.filter (isRelatedPos p)

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



-- * IO
-- analyze :: Matrix Val -> PosMap
-- analyze matrix = S.zip posList (vals matrix)
--   where
--     vals = S.fromList . map choices . concat

-- choices :: Val -> Seq Val
-- choices x
--   | x == 0 = S.fromList [1..boardSize]
--   | otherwise = S.singleton x

-- posList :: Seq Pos
-- posList = cps (1,1) 
--   where
--     cps (r, c)
--       | r > boardSize = S.empty
--       | c > boardSize = cps (r + 1, 1)
--       | otherwise = (r, c) <| cps (r, c + 1)

-- toMatrix :: PosMap -> Matrix Val
-- toMatrix pm = divide boardSize . F.toList . join . fmap snd $ sorted
--   where
--     sorted = S.sortBy (\(p1, _) (p2, _) -> compare p1 p2) pm

-- divide :: Int -> [a] -> [[a]]
-- divide _ [] = []
-- divide n xs = left : divide n right
--   where
--     (left, right) = splitAt n xs
    
-- display :: Matrix Val -> IO ()
-- display = mapM_ (putStrLn . concat . intersperse " " . map show)

-- main :: IO ()
-- main = do
--   -- files <- getArgs
--   -- sudoku <- getSudoku
--   -- let answers = solve sudoku
--   -- mapM_ display answers
--   -- putStrLn "--"
--   -- showSolve easy
--   -- showSolve hard
--   sudoku17
  
-- sudoku17 :: IO ()
-- sudoku17 = forever $ do
--   sudoku <- divide 9 . map read . divide 1 <$> getLine
--   F.mapM_ display $ runSolve sudoku
--   putStrLn "--"
  
-- showSolve :: Matrix Val -> IO ()
-- showSolve = F.mapM_ display . runSolve 

-- runSolve :: Matrix Val -> Seq (Matrix Val)
-- runSolve = fmap toMatrix . solve . eliminate . analyze


    
runSolve :: Sudoku -> Seq (Matrix Val)
runSolve = fmap toMatrix . solve . eliminate . analyze

showSolve :: Sudoku -> IO ()
showSolve = F.mapM_ display . runSolve 


analyze :: Sudoku -> PosMap
analyze (row, col, matrix) = 
  S.zip (cellposList row col) (vals matrix)
  where
    vals = S.fromList . map (\x -> if x == 0 then S.fromList [1..(row * col)] else S.singleton x) . concat

cellposList :: Int -> Int -> Seq CellPos
cellposList rsize csize = cps (1,1) (1,1) 
  where
    cps (br, bc) (r, c)
      | br > csize = S.empty
      | r > rsize = cps (br + 1, 1) (1, 1)
      | bc > rsize = cps (br, 1) (r + 1, 1)
      | c > csize = cps (br, bc + 1) (r, 1)
      | otherwise = ((br, bc), (r, c)) <| cps (br, bc) (r, c + 1)

toMatrix :: PosMap -> Matrix Val
toMatrix pm = divide boardsize . F.toList . join . liftM snd $ sorted
  where
    boardsize = intSqrt (S.length pm)
    sorted = S.sortBy (\(p1, _) (p2, _) -> compare (replace p1) (replace p2)) pm
    replace ((a, b), (c, d)) = ((a, c), (b, d))

intSqrt :: Int -> Int
intSqrt num = intSqrt' num 1
  where
    intSqrt' n m
      | n < m * m = m - 1
      | otherwise = intSqrt' n (m + 1)

-- divide :: Int -> Seq a -> Seq (Seq a)
-- divide n xs
--   | S.null xs = S.empty
--   | otherwise = fit <| divide n rest
--   where
--     (fit, rest) = S.splitAt n xs
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
  




