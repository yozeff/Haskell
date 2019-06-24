--Joseph Harrison 2019
--abstract syntax trees

data Ast = EmptyAst | Node Char (Ast) (Ast)
    deriving (Read, Eq)

instance Show Ast where
    show ast = show_contents ast 0

show_contents :: Ast -> Int -> String
show_contents EmptyAst _ = ""
--concatenate right branch, then current vertex, then left branch
show_contents (Node item left right) level = let leftcontents = show_contents left (level + 5)
                                                 rightcontents = show_contents right (level + 5)
                                                 vertex = (take level $ repeat ' ') ++ (show item)
                                             in rightcontents ++ "\n" ++ vertex ++ "\n" ++ leftcontents

singleton :: Char -> Ast
singleton x = Node x EmptyAst EmptyAst

operators = ['+', '-', '*', '/', '^', '=']

data Result = Success | Failure
    deriving (Eq, Show, Read)

ast_insert :: Char -> Ast -> (Ast, Result)
ast_insert x EmptyAst = (singleton x, Success)
ast_insert x (Node item left right)
    --if the item is a token, this path has failed
    | (item `elem` operators) == False = (ast, Failure)
    --try placing the item right
    | otherwise = let placingright = ast_insert x right
                  in case (snd placingright) of
                      --if data was successfully placed right
                      Success -> (Node item left (fst placingright), Success)
                      --if it failed try placing left
                      Failure -> let placingleft = ast_insert x left
                                 in (Node item (fst placingleft) right, snd placingleft) 
    where ast = Node item left right

ast_insert_items :: Ast -> [Char] -> Ast
ast_insert_items = foldr (\x acc -> fst $ ast_insert x acc)

main = do
    putStr "rpn expr: "
    expr <- getLine
    if (expr == "")
    then return()
        else do
            putStrLn "ast:"
            let ast = ast_insert_items EmptyAst expr
            print ast
            main

