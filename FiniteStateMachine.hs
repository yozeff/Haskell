--Joseph Harrison 2019
--implementation quintessential example of 'state'
--with functional purity and data immutability
module FiniteStateMachine
( transition
, output
, FiniteStateMachine
) where

import Data.List
import Control.Monad

newtype FiniteStateMachine a = FiniteStateMachine a
    deriving (Show)

transition :: a -> FiniteStateMachine b -> (a -> b -> b) -> FiniteStateMachine b
transition x (FiniteStateMachine s) f = FiniteStateMachine (f x s)

output :: a -> FiniteStateMachine b -> (a -> b -> a) -> a
output x (FiniteStateMachine s) g = g x s

--functions for serial adder addition
--transition function
f :: String -> String -> String
f "00" "c"  = "nc"
f "11" "nc" = "c"
f _ s       = s

--output function
g :: String -> String -> String
g "00" "c"  = "1"
g "11" "c"  = "1"
g _    "c"  = "0"
g "00" "nc" = "0"
g "11" "nc" = "0"
g _    "nc" = "1"

--apply serial adder fsm m to input matrix xs using f and g as transition and output functions respectively
serial_adder :: FiniteStateMachine String -> [String] -> (String -> String -> String) -> (String -> String -> String) -> String -> String
serial_adder _ [] _ _ xs = xs
serial_adder m (x:xs) f g ys = let n = transition x m f
                                   y = output x m g
                               in serial_adder n xs f g (y ++ ys)

runtime :: IO ()                          
runtime = do
    let m = FiniteStateMachine "nc"
    input1 <- getLine
    input2 <- getLine
    when (input1 /= "quit" && input2 /= "quit") $ do
        let xs = reverse $ transpose [input1, input2]
        putStrLn $ serial_adder m xs f g ""
        runtime
        
main = do
    putStrLn "enter 'quit' to quit"
    runtime
    putStrLn "exiting..."
