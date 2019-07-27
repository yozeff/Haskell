--Joseph Harrison 2019
import qualified Brainfuck as Bf
import Control.Monad

run :: Bf.ProgState -> IO ()
run s = do
    putStr "$$ "
    p <- getLine
    when (p /= "quit") $ do
        x <- Bf.interpret (Bf.Interpreter 0 p s)
        s <- case x of
                Just y -> return y
                Nothing -> do
                            putStrLn "error"
                            return s
        run s

main :: IO ()
main = do
      putStrLn "Brainfuck repl - Joseph Harrison 2019\n'quit' to quit"
      run (Bf.State 0 $ take 8 $ repeat 0)
      putStrLn "exiting..."
