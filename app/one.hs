import Data.List
import Data.Maybe

-- read file as lines into array
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

testFile = "one_test.txt"
inputFile = "one_input.txt"
simpleCase = "two1nine"
harderCase = "zoneight234"

numberNames = [ ("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9"), ("", "") ]

-- this is a little janky but it _did_ work as i expected
addNumbersToString :: String -> String
addNumbersToString [] = ""
addNumbersToString all@(x:xs) = [x] ++ number ++ addNumbersToString xs
  where number = [num | (name,num) <- numberNames, not (isNothing (stripPrefix name all))]!!0

getNumberFromLine :: String -> Int
getNumberFromLine [] = 0
getNumberFromLine xs = read $ [head nums] ++ [last nums] :: Int
  where nums = [x | x<-xs, x `elem` ['1'..'9']]

process :: [String] -> Int
process xs = sum (map (\ x -> getNumberFromLine (addNumbersToString x)) xs)

execute = readLines inputFile >>= (\list -> return (process list)) :: IO Int
test = readLines testFile >>= (\list -> return (process list)) :: IO Int
