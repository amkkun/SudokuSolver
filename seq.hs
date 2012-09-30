{-# OPTIONS -Wall #-}

import Data.Sequence (Seq, (<|), (><))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List

boardSize :: Int
boardSize = 9
boxSize :: Int
boxSize = 3 
cellvals :: Choices
cellvals = S.fromList [1..9]
blank :: Val -> Bool
blank = (== 0) 


type Matrix a = [[a]]
type Sudoku = Matrix Val

type SeqMap k a = Seq (k, Seq a)

type Choices = Seq Val

type Val = Int
type Pos = (Int, Int)

type PosMap = SeqMap Pos Val
type ValMap = SeqMap Val Pos

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
    pm' = toPosMap . elimLine . elimPos . toValMap . elimDet $ pm

-- * eliminate PosMap
toPosMap :: ValMap -> PosMap
toPosMap vm = do
  pos <- posList
  return (pos, fmap fst $ S.filter (\(_, poss) -> F.elem pos poss) vm)

-- * eliminate determinate value
elimDeterminate :: PosMap -> PosMap
elimDeterminate pm = pm >>= return . elimDeterminate' determinate
  where
    determinate = S.filter (isSingle . snd) pm

elimDeterminate' :: PosMap -> (Pos, Seq Val) -> (Pos, Seq Val)
elimDeterminate' pm pair@(pos, vals)
  | isSingle $ snd pair = pair
  | otherwise = (pos, filterDup relatedVals vals)
  where
    relatedPM = S.filter (isRelatedPos pos . fst) pm 
    relatedVals = join $ liftM snd relatedPM

elimDet :: PosMap -> PosMap
elimDet = op isSameCol . op isSameRow . op isSameBox
  where
    op f = join . liftM reduceDet . subGroupPM f

subGroupPM :: (Pos -> Pos -> Bool) -> PosMap -> Seq PosMap
subGroupPM f = grouping (\(p1, _) (p2, _) -> f p1 p2)
  
reduceDet :: PosMap -> PosMap
reduceDet pm = liftM (\(p, vs) -> (p, if isSingle vs
                                      then vs
                                      else filterDup detVals vs)) pm
  where
    detVals = join $ S.filter isSingle $ liftM snd pm
    
-- * eliminate ValMap
toValMap :: PosMap -> ValMap
toValMap pm = do
  val <- cellvals
  return (val, fmap fst $ S.filter (\(_, vals) -> F.elem val vals) pm)

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
  
subGroup :: (Pos -> Pos -> Bool) -> ValMap -> Seq ValMap
subGroup isSame vm = grouping samePos tmp 
  where
    tmp = do
      (val, poss) <- vm
      poss' <- grouping isSame poss
      return (val, poss')
    samePos (_, ps) (_, ps') = isSame (S.index ps 0) (S.index ps' 0) 

-- * eliminate Line
elimLine :: ValMap -> ValMap
elimLine = fmap (\(v, ps) -> (v, (reduceLine isSameCol . reduceLine isSameRow) ps))

reduceLine :: (Pos -> Pos -> Bool) -> Seq Pos -> Seq Pos
reduceLine f ps = filter2 cond ps fit
  where
    fit = join . liftM (liftM (`S.index` 0)) . S.filter isSingle . liftM (grouping f) . grouping isSameBox $ ps
    cond p1 p2 = isSameBox p1 p2 || not (f p1 p2)

-- * eliminate determinate position
elimDetBox :: ValMap -> ValMap
elimDetBox vm = joinVM $ do
  box <- subGroup isSameBox vm
  let detPos = join . S.filter isSingle . liftM snd $ box
      box' = liftM (\(v, ps) -> (v, if isSingle ps
                                    then ps
                                    else filterDup detPos ps)) box
  return box'

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
takeRelatedPos :: Pos -> Seq Pos -> Seq Pos
takeRelatedPos p = S.filter (isRelatedPos p)

isRelatedPos :: Pos -> Pos -> Bool
isRelatedPos a b = a /= b &&
                   (isSameBox a b || isSameRow a b || isSameCol a b)

isSameBox :: Pos -> Pos -> Bool
isSameBox a b = box a == box b
  where
    box (r, c) = ((r - 1) `div` boxSize) * boxSize +
                 (c - 1) `div` boxSize 

isSameRow :: Pos -> Pos -> Bool
isSameRow a b = fst a == fst b

isSameCol :: Pos -> Pos -> Bool
isSameCol a b = snd a == snd b





-- * IO
analyze :: Matrix Val -> PosMap
analyze matrix = S.zip posList (vals matrix)
  where
    vals = S.fromList . map choices . concat

choices :: Val -> Seq Val
choices x
  | blank x = S.fromList [1..boardSize]
  | otherwise = S.singleton x

posList :: Seq Pos
posList = cps (1,1) 
  where
    cps (r, c)
      | r > boardSize = S.empty
      | c > boardSize = cps (r + 1, 1)
      | otherwise = (r, c) <| cps (r, c + 1)

toMatrix :: PosMap -> Matrix Val
toMatrix pm = divide boardSize . F.toList . join . fmap snd $ sorted
  where
    sorted = S.sortBy (\(p1, _) (p2, _) -> compare p1 p2) pm

divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = left : divide n right
  where
    (left, right) = splitAt n xs
    
display :: Matrix Val -> IO ()
display = mapM_ (putStrLn . concat . intersperse " " . map show)

main :: IO ()
main = do
  -- files <- getArgs
  -- sudoku <- getSudoku
  -- let answers = solve sudoku
  -- mapM_ display answers
  -- putStrLn "--"
  -- showSolve easy
  -- showSolve hard
  sudoku17
  
sudoku17 :: IO ()
sudoku17 = forever $ do
  sudoku <- divide 9 . map read . divide 1 <$> getLine
  F.mapM_ display $ runSolve sudoku
  putStrLn "--"
  
showSolve :: Matrix Val -> IO ()
showSolve = F.mapM_ display . runSolve 

runSolve :: Matrix Val -> Seq (Matrix Val)
runSolve = fmap toMatrix . solve . eliminate . analyze

