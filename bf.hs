--Joseph Harrison 2019
module Brainfuck
( ProgState
, Interpreter
, interpret
) where

import qualified Data.Char as Char

data ProgState = State Int [Int] deriving Show
type Prog = String
type Token = Char

mut_dp :: ProgState -> (Int -> Int) -> ProgState
mut_dp (State d c) f = State (f d `mod` length c) c

mut_cells :: ProgState -> (Int -> Int) -> ProgState
mut_cells (State d c) f = State d (map g $ zip [0..] c)
      where g :: (Int, Int) -> Int
            g (i, x) = if i == d then f x else x

out_cell :: ProgState -> IO ()
out_cell (State d c) = print $ Char.chr $ c !! d

inp_cell :: ProgState -> IO ProgState
inp_cell (State d c) = do
                        putStr "$ "
                        x <- getLine
                        return $ mut_cells (State d c) (\_ -> Char.ord $ head x)

next_token :: Prog -> Token -> Token -> Int -> Int -> (Int -> Int) -> Maybe Int
next_token p t u i j f
      | i == length p = Nothing
      | (p !! i) == t && j == 1 = Just i
      | (p !! i) == t = next_token p t u (f i) (j - 1) f
      | (p !! i) == u = next_token p t u (f i) (j + 1) f
      | otherwise = next_token p t u (f i) j f

data Interpreter = Interpreter Int Prog ProgState deriving Show

interpret :: Interpreter -> IO (Maybe ProgState)
interpret (Interpreter i p (State d c))
      | i == length p = do return $ Just s
      | otherwise = case p !! i of
                        '+' -> interpret $ Interpreter (i + 1) p $ mut_cells s (+1)
                        '-' -> interpret $ Interpreter (i + 1) p $ mut_cells s (\x -> x - 1)
                        '>' -> interpret $ Interpreter (i + 1) p $ mut_dp s (+1)
                        '<' -> interpret $ Interpreter (i + 1) p $ mut_dp s (\x -> x - 1)
                        '.' -> do
                              out_cell s
                              interpret $ Interpreter (i + 1) p s
                        ',' -> do
                              x <- inp_cell s
                              interpret $ Interpreter (i + 1) p $ x
                        '[' -> case c !! d of
                                    0 -> case next_token p ']' '[' i 0 (+1) of
                                                Just x -> interpret $ Interpreter x p s
                                                Nothing -> return $ Nothing
                                    _ -> interpret $ Interpreter (i + 1) p s
                        ']' -> case c !! d of
                                    0 -> interpret $ Interpreter (i + 1) p s
                                    _ -> case next_token p '[' ']' i 0 (\x -> x - 1) of
                                                Just x -> interpret $ Interpreter x p s
                                                Nothing -> return $ Nothing
                        _ -> return $ Nothing
      where s = State d c

main = interpret (Interpreter 0 ",>,<[->+<]>." (State 0 [0, 0]))
