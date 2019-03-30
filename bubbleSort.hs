--Joseph Harrison 2019
--bubble sort algorithm

--swap list element at p with its predecessor
swap :: [Int] -> Int -> [Int]
swap list p = take (p - 1) list ++ [list !! p] ++ [list !! (p - 1)] ++ drop (p + 1) list

--one iteration of the bubble sort algorithm
iteration :: [Int] -> Int -> [Int]
iteration list p =
  if p == length list
    then list
      else if list !! (p - 1) > list !! p
      then iteration (swap list p) (p + 1)
      else iteration list (p + 1)

--check list to see if it's sorted
check :: [Int] -> Int -> Bool
check list p =
  if p == length list
    then True
    else if list !! (p - 1) > list !! p
      then False
      else check list (p + 1)

--full bubble sort calls iteration on list until it's sorted
bubble_sort :: [Int] -> [Int]
bubble_sort list =
  if check list 1 == True
    then list
    else bubble_sort (iteration list 1)

main = do
  print [2,4,1,5,7,2,1,7,3,5,3,7,3,5,3,3,22,4,5,7,4,2,2]
  print (bubble_sort [2,4,1,5,7,2,1,7,3,5,3,7,3,5,3,3,22,4,5,7,4,2,2])