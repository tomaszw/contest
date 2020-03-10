module Main where

import Data.Maybe
import Data.List
import qualified Sim as S
import Random
import Control.Applicative
import Control.Monad
import qualified Data.Permute as P
import qualified Data.Map as M

data Gene   = X | L Int | R Int deriving (Eq,Show,Ord)
type Genome = [Gene]
type Gate1  = (Int, Gene, Gene)

sample1 = [X, L 0]
sample2 = [L 1, X, L 0, R 0]

domain n   = X : inputsDB n
imapping n = M.fromList $ zip (domain n) [0..]
imapping_rev n = M.fromList $ zip [0..] (domain n)

genome2i :: Genome -> [Int]
genome2i g = 
    map g2i g
  where g2i (L i) = 2*i
        g2i (R i) = 2*i + 1
        g2i X     = l

i2genome :: Int -> [Int] -> Genome
i2genome n =
    map i2g
  where
    i2g i = fromJust $ M.lookup i (imapping_rev n)

genome2Perm :: Int -> Genome -> P.Permute
genome2Perm n g =
    P.listPermute n (genome2i n g)

perm2Genome :: Int -> P.Permute -> Genome
perm2Genome n p =
    i2genome n . P.elems $ p


parseGenome :: Genome -> S.Factory
parseGenome g =
    let gates1 = gates1OfGenome 0 g
        gates  = gatesOfGenome 0 gates1 g
    in
      S.factoryOfGates gates

gates1OfGenome :: Int -> Genome -> [Gate1]
gates1OfGenome id (p:q:xs) =
    (id,p,q) : gates1OfGenome (id+1) xs
gates1OfGenome id _ = []

gatesOfGenome :: Int -> [Gate1] -> Genome -> [S.Gate]
gatesOfGenome id fullG (p:q:xs) =
    gate : gatesOfGenome (id+1) fullG xs
  where
    gate = S.Gate (lineL p) (lineR q) (findL id) (findR id)
    lineL X     = S.X
    lineL (L i) = S.Line i S.L 
    lineL (R i) = S.Line i S.L
    lineR X     = S.X
    lineR (L i) = S.Line i S.R 
    lineR (R i) = S.Line i S.R
    
    findL id = matches id (matchL id)
    findR id = matches id (matchR id)

    matchL i (x, L j, _) | i == j = Just (S.Line x S.L)
    matchL i (x, _, L j) | i == j = Just (S.Line x S.R)
    matchL _ _                    = Nothing
    matchR i (x, R j, _) | i == j = Just (S.Line x S.L)
    matchR i (x, _, R j) | i == j = Just (S.Line x S.R) 
    matchR _ _                    = Nothing

    matches :: Int -> ((Int,Gene,Gene)->Maybe S.Line) -> S.Line
    matches id mfun = case filter isJust . map mfun $ fullG of
                        (Just x : _) -> x
                        _            -> S.X
gatesOfGenome _ _ _ = []    


-- all inputs for problem of size n
inputsDB n = [L i | i <- [0..n-1]] ++ [R i | i <- [0..n-1]]
cut n xs   = let (a,b) = splitAt n xs in
             ((case b of [] -> error "asdf"
                         _  -> head b), a ++ tail b)

--allGenomes n = permutations $ X : inputsDB n

insertAt n x xs = let (a,b) = splitAt n xs in
                  a ++ [x] ++ b

randomGenome n =
    do let inputs = inputsDB n
       c <- randomRIO (0, length inputs - 1)
       let inputs'= snd $ cut c inputs
       g <- randomGenome_ inputs' (length inputs')
       r <- randomRIO (0, length g - 1)
       return $ insertAt r X g

randomGenome_ [] _ = return []
randomGenome_ _  0 = return []
randomGenome_ inputs len = do
    cutP <- randomRIO (0,len-1)
    let (x,inputs') = cut cutP inputs
    xs <- randomGenome_ inputs' (len-1)
    return (x : xs)

genPopulation n 0    = return []
genPopulation n size = (:) <$> randomGenome n <*> genPopulation n (size-1)

evalPopulation :: [Genome] -> [(Int, Genome)]
evalPopulation genomes = map eval genomes
                         where eval g = (fitness g, g)

rangeFitness :: Int -> [(Int, Genome)] -> [(Int,Int,Genome)]
rangeFitness x []               = []
rangeFitness x ((fit,g) : rest) =
    (x,x+fit,g) : rangeFitness (x+fit+1) rest

roulette :: [(Int,Int,Genome)] -> IO Genome
roulette genomes = do
    r <- randomRIO (0, maxF)
    return $ select r genomes
  where
    maxF           = maximum (map upperB genomes)
    upperB (_,u,_) = u
    select r ((a,b,g) : rest) | r >= a && r <= b   = g
                              | otherwise          = select r rest

reproductionPool :: Int -> [Genome] -> IO [Genome]
reproductionPool size genomes = do
    get size
  where
    evaluated = rangeFitness 0 (evalPopulation genomes)
    get 0     = return []
    get n     = do g  <- roulette evaluated
                   gs <- get (n-1)
                   return (g:gs)

generations 0 genomes = return genomes
generations n genomes = do g' <- generation genomes
                           putStr "."
                           g' `seq` generations (n-1) g'

generation :: [Genome] -> IO [Genome]
generation genomes =
    generation' size
  where

    evaluated   = rangeFitness 0 (evalPopulation genomes)
    size        = length genomes

    generation' 0     = return []
    generation' count = (:) <$> mate <*> generation' (count-1)

    mate        = do mates <- chooseMates
                     return $ fst mates --cross 4 mates

    cross (a,b) = crossover a b

    chooseMates = do a <- roulette evaluated
                     b <- roulette evaluated
                     return (a,b)

elite' :: Int -> [Genome] -> [(Int, Genome)]
elite' n pop =
    elite n . evalPopulation $ pop

elite :: Int -> [(Int, Genome)] -> [(Int, Genome)]
elite n genomes =
    take n $ sortBy cmp genomes
  where
    cmp a b = compare (fst b) (fst a)

crossover n genome1 genome2 = do
    let cycles = cyc_a ++ cyc_b
        l      = length cycles
    return $ perm2Genome n $ P.cyclesPermute (length genome1) cyc_a
    --r <- randomRIO (1, l)
    --c <- shuffle cycles
    --let c' = take r c
    --return $ perm2Genome $ P.cyclesPermute (length genome1) c'
  where
    cyc_a = P.cycles $ genome2Perm n genome1
    cyc_b = P.cycles $ genome2Perm n genome2

fitness genome =
    let f = parseGenome genome
        s = score f
    in s

shuffle :: [a] -> IO [a]
shuffle l = shuffle' l []
  where
    shuffle' [] acc = return acc
    shuffle' l acc =
        do k <- randomRIO (0, length l - 1)
           let (lead, x:xs) = splitAt k l
           shuffle' (lead ++ xs) (x:acc)

maxFitness = 17

score circuit = score_ circuit [1,1,0,2,1,2,1,0,1,1,2,1,0,1,2,2,1]
score_ circuit out' =
    length $ filter eq $ zip out out'
  where
    eq (a,b) = a == b
    out  = S.runSim circuit S.inputString

main = do
  p  <- genPopulation 20 500
  p' <- generations 20 p
  --putStrLn $ show p'
  let e  = elite' 1 p'
  putStrLn ""
  putStrLn $ show e