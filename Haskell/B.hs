import Data.List
import Data.Char
import Data.Maybe

type FreqTable = [(Char, Int)]
rareChars :: FreqTable -> Int -> [Char]
rareChars table n = map (\(char, num) -> char) $ filter (\(char, num) -> num < n) table

insChar :: Char -> FreqTable -> FreqTable
insChar char table = case lookup char table of
    Nothing -> (char, 1):table
    (Just num) -> (char, (num + 1)):(filter (\(charx, _) -> charx /= char) table)

textToStr :: String -> String
textToStr txt = concat $ nub $ words txt

textToTable :: String -> FreqTable
textToTable txt = foldr insChar [] $ textToStr txt