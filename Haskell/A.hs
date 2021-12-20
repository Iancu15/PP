import Data.List
import Data.Char
import Data.Maybe

type Text = String
type Pair = (String, Int)
type Index = [(String, [Int])]

textToLines :: Text -> [(Int, Text)]
countLines :: Num t => t -> [b] -> [(t, b)]
countLines _ [] = []
countLines index (elem:restList) = (index, elem) : countLines (index + 1) restList
textToLines text = countLines 1 $ lines $ map toLower $ filter (`notElem` ".,!?") text

insPair :: Pair -> Index -> Index
insPair pair index = insPairHelper pair index False
insPairHelper :: (String, Int) -> [(String, [Int])] -> Bool -> [(String, [Int])]
insPairHelper _ [] True = []
insPairHelper (line, lineNumb) [] False = [(line, [lineNumb])]
insPairHelper pair@(line, lineNumb) ((indexLine, indexList):rest) bool
    | line == indexLine && (lineNumb `notElem` indexList) = (indexLine, lineNumb:indexList):insPairHelper pair rest True
    | line == indexLine && (lineNumb `elem` indexList) = (indexLine, indexList):insPairHelper pair rest True
    | otherwise = (indexLine, indexList):insPairHelper pair rest bool

allPairsLine :: (Int, String) -> Index
allPairsLine (number, line) = foldl insPair [] (map (\x -> (number, x)) (words line))
allPairs :: Text -> [Pair]
allPairs txt = foldl insPair 