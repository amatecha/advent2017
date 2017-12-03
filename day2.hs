import Data.List (sort)

stringsToInts :: [String] -> [Integer]
stringsToInts xs = map read xs

parseInput :: String -> [[Integer]]
parseInput x = map stringsToInts (map words (lines x))


-- Part 1
solutionPart1 = do
  input <- readFile "day2input.txt"
  print $ sum $ map (\xs -> (maximum xs) - (minimum xs)) $ parseInput input


-- Part 2

-- does it divide evenly (excluding same number!)
isFactor :: Integer -> Integer -> Bool
isFactor x y = if ((rem x y) == 0) && (div x y /= 1) then True else False


-- take a number (x) , divide it by every number in a list (xs) and give the result if there is no remainder (otherwise 0)
iter :: Integer -> [Integer] -> Integer
iter x xs = if (length y) > 0 then div x (head y) else 0
  where y = filter (isFactor x) xs

getResult :: [Integer] -> Integer
getResult xs = head (filter (\x -> x > 0) (map findDivisor xs))
  where findDivisor = (flip iter) xs

solutionPart2 = do
  input <- readFile "day2input.txt"
  print $ sum $ map getResult $ parseInput input