{-# LANGUAGE NoMonomorphismRestriction #-}
module Sim (
             Factory
           , Place (..)
           , Line (..)
           , Gate (..)
           , runSim
           , factoryOfGates
           , inputString
           )
where
                   
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
-- import Data.Foldable
import Control.Arrow
import Control.Monad.State
import qualified Data.DList as D
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Set as S
import Data.List


-- data Node = String deriving (Show, Ord, Eq)
-- data Edge = Edge Place Node Node

alpha = [0,1,2]

-- prod = (<*>) . (<$>) (,)
-- prod' = (,) <$> [1,2] <*> [1,2]


input = alpha -- (,) <$> alpha <*> alpha
output = input

-- allF :: Ord input =>  [input] -> [output] -> [M.Map input output]
-- allF inputs outputs = foldr (=<<) [M.empty] . map t $ inputs
--     where t input m = map (\output -> M.insert input output m) outputs
--main = do return ()
--          print $ fst $(first D.toList foo)

type Factory = (Line, M.Map Int Gate, Line) -- deriving (Show, Ord, Eq)
data Place = L | R deriving (Show, Ord, Eq)
data Line = X | Line Int Place deriving (Show, Ord, Eq)
data Gate = Gate Line Line Line Line deriving (Show, Ord, Eq)

type Alpha = Int

oneTurn (xi, fact, xo) input = do mapM oneGate $ [0..n]
                                  getI xo >>= (tell . D.singleton)
    where n = maximum . ((0-1):) . M.keys $ fact
          oneGate i = let Gate li ri lo ro = M.findWithDefault undefined i fact in
                      do liS <- getI li
                         riS <- getI ri
                         let (loS, roS) = gf liS riS
                         putO (Line i L) loS
                         putO (Line i R) roS
          getI line = case line of
                        X -> lift (return (input :: Alpha))
                        line -> lift (gets (M.findWithDefault defaultLine line))
          putO line v = lift (modify . update line $ v)

          update line v = M.insert line v

defaultLine :: Alpha
defaultLine = 0
simulate (xi, fact, xo) inputs = mapM (oneTurn (xi, fact, xo)) inputs

gf 0 0 = (,) 0 2
gf 0 1 = (,) 2 2
gf 0 2 = (,) 1 2

gf 1 0 = (,) 1 2
gf 1 1 = (,) 0 0
gf 1 2 = (,) 2 1

gf 2 0 = (,) 2 2
gf 2 1 = (,) 1 1
gf 2 2 = (,) 0 0 

rgf 0 2 = (,) 0 0
rgf 2 2 = (,) 0 1
rgf 1 2 = (,) 0 2
rgf 1 2 = (,) 1 0
rgf 0 0 = (,) 1 1
rgf 2 1 = (,) 1 2
rgf 2 2 = (,) 2 0
rgf 1 1 = (,) 2 1
rgf 0 0 = (,) 2 2 



f3 = (Line 0 R
     ,M.fromList [(0, Gate (Line 1 L) X          (Line 1 L) (Line 1 R))
                 ,(1, Gate (Line 0 L) (Line 0 R) (Line 0 L) X)]
     , Line 1 R)

inputString = [0,1,2,0,2,1,0,1,2,1,0,2,0,1,2,0,2]

f0 = (X, M.fromList [], X)
-- 0R: 0LX0#0LX:0R
f1 = (Line 0 R, M.fromList [(0, Gate (Line 0 L) X (Line 0 L) X)], Line 0 R)
-- foo :: WriterT (D.DList Alpha) (State (M.Map Line Alpha)) ()
--foo :: StateT (M.Map Line Alpha) (Writer (D.DList Alpha)) ()
--foo = runState (execWriterT (simulate sample sampleInput)) M.empty
-- foo' = evalStateT (runWriterT $ oneTurn f0 0) M.empty
-- foo'' = (evalStateT (oneTurn f0 0) (M.empty))

circuit1 = (Line 0 R
           , M.fromList . zip [0..] $
            [ Gate (Line 0 L) X (Line 0 L) X ]
           ,Line 0 R)

sample = (Line 19 L
         ,M.fromList . zip [0..] $
          [ Gate (Line 12 R) (Line 13 R) (Line 1 R) (Line 12 R)
          , Gate (Line 14 R) (Line 0 L)  (Line 4 R) (Line 9 L)
          , Gate (Line 9 R) (Line 10 R)  (Line 3 L) (Line 8 L)
          , Gate (Line 2 L) (Line 17 R)  (Line 5 L) (Line 9 R)
          , Gate (Line 15 R) (Line 1 L) (Line 10 R) (Line 13 R)
          , Gate (Line 3 L) (Line 18 R) (Line 6 L) (Line 15 L)
          , Gate (Line 5 L) (Line 11 R) (Line 13 L) (Line 12 L)
          , Gate (Line 19 R) (Line 16 R) (Line 11 R) (Line 8 R)
          , Gate (Line 2 R) (Line 7 R) (Line 11 L) (Line 10 L)
          , Gate (Line 1 R) (Line 3 R) (Line 18 L) (Line 2 L)
          , Gate (Line 8 R) (Line 4 L) (Line 16 L) (Line 2 R)
          , Gate (Line 8 L) (Line 7 L) (Line 15 R) (Line 6 R)
          , Gate (Line 6 R) (Line 0 R) (Line 14 L) (Line 0 L)
          , Gate (Line 6 L) (Line 4 R) (Line 14 R) (Line 0 R)
          , Gate (Line 12 L) (Line 13 L) (Line 17 L) (Line 1 L)
          , Gate (Line 5 R) (Line 11 L) (Line 16 R) (Line 4 L)
          , Gate (Line 10 L) (Line 15 L) (Line 17 R) (Line 7 R)
          , Gate (Line 14 L) (Line 16 L) (Line 18 R) (Line 3 R)
          , Gate (Line 9 L) (Line 17 L) (Line 19 R) (Line 5 R)
          , Gate X (Line 18 L) X (Line 7 L)]
         , Line 19 L)

possLines numG = X : [Line x d | x <- [0..numG-1], d <- [L,R]]
possGates numG = [Gate a b c d | a <- possLines numG,
                                 b <- possLines numG,
                                 c <- possLines numG,
                                 d <- possLines numG,
                                 b /= a,
                                 d /= c]

oneElem [] = False
oneElem [x] = True
oneElem _ = False


line2Tup (Line num d) = Just (num,d)
line2Tup _            = Nothing


validC :: [Gate] -> Bool
validC c = oneElem inpX && oneElem outX && cond
           where inpX = filter isInpX c
                 outX = filter isOutX c
                 isInpX (Gate X _ _ _) = True
                 isInpX (Gate _ X _ _) = True
                 isInpX (Gate _ _ _ _) = False
                 isOutX (Gate _ _ X _) = True
                 isOutX (Gate _ _ _ X) = True
                 isOutX (Gate _ _ _ _) = False

                 gateITups (Gate l1 l2 _ _) = 
                     map fromJust . filter (/= Nothing) . map line2Tup $ [l1,l2]

                 gateOTups (Gate _ _ l1 l2) = 
                     map fromJust . filter (/= Nothing) . map line2Tup $ [l1,l2]

                 allITups = concat . map gateITups $ c
                 allOTups = concat . map gateOTups $ c
                 
                 cond = S.fromList allITups == S.fromList allOTups

availLines n = X : [Line i d | i <- [0..n-1], d <- [L,R]]

possC 1 = [[Gate a b c d] | a <- availLines 1,
                            b <- availLines 1, b /= a,
                            c <- availLines 1, 
                            d <- availLines 1, d /= c]
                              

          
possCircs 1 = filter validC [[a] | a <- possGates 1]
possCircs 2 = filter validC [[a,b] | a <- possGates 2, b <- possGates 2]
possCircs 3 = filter validC [[a,b,c] | a <- possGates 3, b <- possGates 3, c <- possGates 3]

factoryOfGates :: [Gate] -> Factory
factoryOfGates c =
    (entry c, M.fromList (gates c), exi c)
  where
    gates :: [Gate] -> [ (Int,Gate) ]
    gates c = zip [0..] c
    entry c = head $ map fromJust . filter isJust . map findXInp $ gates c
    exi   c = head $ map fromJust . filter isJust . map findXOut $ gates c

    findXInp (id, Gate X _ _ _) = Just $ Line id L
    findXInp (id, Gate _ X _ _) = Just $ Line id R
    findXInp _                  = Nothing
    findXOut (id, Gate _ _ X _) = Just $ Line id L
    findXOut (id, Gate _ _ _ X) = Just $ Line id R
    findXOut _                  = Nothing

possCircs' n =
    map (\c -> (entry c, M.fromList (gates c), exi c)) pc
  where
    pc    = possCircs n
    gates :: [Gate] -> [ (Int,Gate) ]
    gates c = zip [0..] c
    entry c = head $ map fromJust . filter isJust . map findXInp $ gates c
    exi   c = head $ map fromJust . filter isJust . map findXOut $ gates c

    findXInp (id, Gate X _ _ _) = Just $ Line id L
    findXInp (id, Gate _ X _ _) = Just $ Line id R
    findXInp _                  = Nothing
    findXOut (id, Gate _ _ X _) = Just $ Line id L
    findXOut (id, Gate _ _ _ X) = Just $ Line id R
    findXOut _                  = Nothing

sampleInput = [0,2,2,2,2,2,2,0,2,1,0,1,1,0,0,1,1]

score circuit = score_ circuit [1,1,0,2,1,2,1,0,1,1,2,1,0,1,2,2,1]

score_ circuit out' =
    length $ filter eq $ zip out out'
  where
    eq (a,b) = a == b
    out  = runSim circuit inputString
    
runSim circuit input = fst $ (first D.toList foox)
                       where foox = runState (execWriterT (simulate circuit input)) M.empty

results =
    maximumBy cmp $ zip circuits scores
   where
     cmp (_,a) (_,b) = compare a b
     circuits = possCircs' 3
     scores   = map score circuits

--main = putStrLn $ show results
