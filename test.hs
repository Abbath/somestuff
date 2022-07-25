import Data.List
import Data.Char
import Control.Monad.State
import Distribution.Compat.CharParsing (letter)
import Control.Concurrent.STM (check)

data Configuration = Conf { 
    word :: String,
    letters :: String,
    noletters :: String,
    allowed :: [String],
    answers :: [String]
} deriving Show

defaultConfiguration :: Configuration
defaultConfiguration = Conf {
    word = "?????",
    letters = "",
    noletters = "",
    allowed = [],
    answers = []
}

readWords :: FilePath -> IO [String]
readWords filename = lines <$> readFile filename

readAllowed :: IO [String]
readAllowed = readWords "../allowed.txt"

readAnswers :: IO [String]
readAnswers = readWords "../answers.txt"

readInput :: IO (String, String)
readInput = (,) <$> getLine <*> getLine

checkInput :: (String, String) -> Either String [(Char, Char)]
checkInput (a, b) = if length a == 5 && length b == 5 && all isAlpha a && all (`elem` "*+?") b
    then Right $ zip a b 
    else Left "Wrong input"

processInput :: (String, String, String) -> [(Char, Char)] -> Either String (String, String, String)
processInput r [] = Right r
processInput (w, l, n) ((c, '*'):xs)  = processInput  (w <> ['?'], l, c:n) xs
processInput (w, l, n) ((c, '?'):xs)  = processInput  (w <> ['?'], c:l, n) xs
processInput (w, l, n) ((c, '+'):xs)  = processInput  (w <> [c], l, n) xs 
processInput _ r = Left "Wrong input" 

getAnswers :: [String] -> String -> [String]
getAnswers as w = filter (all (uncurry (==)) . filter (\(x, _) -> x /= '?') . zip w) as

f :: Int -> State Int Int
f y = do
    x <- get
    modify (+y)
    return $ x + y 

main = do
    allowed_ <- readAllowed
    answers_ <- readAnswers
    let dc = defaultConfiguration {allowed = allowed_, answers = answers_}
    Right test <- (checkInput >=> processInput ("", "", "")) <$> readInput
    print test
    let (w, l, n) = test
    print $ getAnswers (answers dc) w
