--Victor Longberg, viclon-8
type Sublist = [(Int,Int,Int,[Int])]
type IochJ = ([Int] -> [[Int]])
type MainList = [Int]

--Apply iSort to the list of tuples so that they come in order of Size. Low -> High
arrayOfTuplesSorting :: IochJ -> MainList -> Sublist
arrayOfTuplesSorting func list = iSort(arrayOfSublists(func list) list)

--Here i make the list of tuples of Sum, i, j & the Sublist
arrayOfSublists :: [[Int]] -> MainList -> Sublist
arrayOfSublists (x:xs) list  
    | null xs    = [(sum (calcSublist x list) , i x , j x , calcSublist x list)]
    | otherwise  =  (sum (calcSublist x list) , i x , j x , calcSublist x list) : arrayOfSublists xs list

--Takes the input list and creates all the possible i and j combinations for the sublists.
ioj :: IochJ
ioj lst = [[i,j] | i <-[1..length lst], j <-[1..length lst], i<=j]

-- *************************************************************************
-- Takes header from the list of integers from ioj, we take the header of that list and calculate the sublist. 
-- I = i - 1
-- J = j - i - 1
calcSublist :: [Int] -> [a] -> [a]
calcSublist (x:xs) lst = take (head xs -(x-1)) (drop (x-1) lst)

-- Takes the first element from the head from array x made from function ioj. 
i :: [a] -> a
i   = head 

-- Takes the head of the tail from the array x made from function ioj. 
j :: [a] -> a
j (x:xs) = head xs

--Taken/Inspired from lecture notes 3
-- Sorts the array of tuples
ins :: Ord t => t -> [t] -> [t]
ins x [] = [x]
ins x (y : ys) = if x <= y then x : y : ys else y : ins x ys 

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort xs = foldr ins [] xs
-- *************************************************************************

-- Print functions:
-- Print function to show value representation
printTxtValue :: IO ()
printTxtValue = putStr ("Size:" ++  " \ti: "  ++ " \tj:" ++  " \tsublist: \n" )

-- Transforms each tuple in list of tuples to strings so they can be printed
t2s :: (Show a1, Show a2, Show a3, Show a4) => (a1, a2, a3, a4) -> [Char]
t2s (size, i, j, sublist) =  show size ++ "\t" ++ show i ++ "\t" ++ show j ++ "\t" ++ show sublist ++ "\n" 

--Print/help function for smallest k set.
smallestKset :: [Int] -> Int -> IO ()
smallestKset xs k = putStrLn ( concatMap t2s(take k(arrayOfTuplesSorting ioj xs))) 

main :: IO()
main = do
    printTxtValue
    smallestKset [-1,2,-3,4,-5] 3

    printTxtValue
    smallestKset [x*(-1)^x| x<-[1..100]] 15

    printTxtValue
    smallestKset [24,-11,-34,42,-24,7,-19,21] 6

    printTxtValue
    smallestKset [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3] 8
