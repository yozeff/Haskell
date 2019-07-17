--Joseph Harrison 2019

type Matrix a = [[a]]

transpose :: Matrix a -> Matrix a
transpose ([]:_) = []
transpose xss = map head xss : (transpose $ map tail xss)

--deconstructs tuple args from zip function
--multiplies contents of tuples
f :: Num a => (a, a) -> a
f (x, y) = x * y

dot :: Num a => [a] -> [a] -> a
dot xs = sum . map f . zip xs

mult :: Num a => Matrix a -> Matrix a -> Matrix a
--when we have exhausted the first arg with recursion return empty list
mult [] _ = []
--dot top row of (xs:xss) with each col of yss (row of transpose of yss)
--this creates a new list which is consed onto a recursive call with xss
mult (xs:xss) yss = (map (dot xs) $ transpose yss) : (mult xss yss)

main = do
    let a = [[1, 2], [3, 4]]
    let b = [[5, 4, 1], [3, 2, 2]]
    let v = [[1], [2]]
    print $ mult a b
    print $ mult a v