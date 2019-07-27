--Joseph Harrison 2019
import qualified Brainfuck as Bf
import System.Environment
import System.IO
import System.Directory

main = do
      args <- getArgs

      if length args /= 1 then
            putStrLn "single arg not given"

      else do

            let filename = args !! 0

            exists <- doesFileExist filename

            if exists then do

                  contents <- readFile filename
                  --get program
                  let p = filter (\x -> x /= '\n' && x /= ' ') contents

                  --make initial state
                  let s = Bf.State 0 $ take 8 $ repeat 0

                  --interpret program print final state or error
                  x <- Bf.interpret (Bf.Interpreter 0 p s)
                  case x of
                        Just x -> print x
                        Nothing -> putStrLn "error"

            else

                  putStrLn $ "file " ++ filename ++ " does not exist"  
