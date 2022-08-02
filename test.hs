import Data.List ( group, sort )
import Data.Char ( isAlpha )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad ( (>=>) )
import System.IO ( hFlush, stdout )
import System.Environment (getArgs)

type Letters = Map Char (Int, Set Int)

readWords :: FilePath -> IO [String]
readWords filename = lines <$> readFile filename

readInput :: IO (String, String)
readInput = (,) <$> getLine <*> getLine

checkInput :: (String, String) -> Either String [(Char, Char)]
checkInput (a, b) = if length a == 5 && length b == 5 && all isAlpha a && all (`elem` "*+?") b
    then Right $ zip a b 
    else Left "Wrong input"

processInput :: Int -> (String, Letters, Set Char) -> [(Char, Char)] -> Either String (String, Letters, Set Char)
processInput i (w, l, n) [] = Right (w, l, Set.filter (\k -> not $ Map.member k l) n)
processInput i (w, l, n) ((c, '*'):xs)  = processInput (i+1) (w <> ['?'], l, Set.insert c n) xs
processInput i (w, l, n) ((c, '?'):xs)  = processInput (i+1) (w <> ['?'], Map.insertWith inserter c (1, Set.singleton i) l, n) xs
processInput i (w, l, n) ((c, '+'):xs)  = processInput (i+1) (w <> [c], l, n) xs
processInput _ _ r = Left "Wrong input"
    
inserter :: (Int, Set Int) -> (Int, Set Int) -> (Int, Set Int)
inserter (val, set) (new_val, new_set) = (val + new_val, Set.union set new_set)

getAnswers :: [String] -> String -> [String]
getAnswers as w = filter (all (uncurry (==)) . filter (\(x, _) -> x /= '?') . zip w) as

filterAnswers :: (String, Letters, Set Char) -> [String] -> [String]
filterAnswers (w, l, n) = filter (\a -> all (`elem` a) (Map.keysSet l) && 
                                        matchNotLetters n w a && 
                                        matchWords a w && 
                                        matchLetters l a)

mapify :: String -> Map Char Int
mapify = Map.fromList . map (\x -> (head x, length x)) . group . sort

mergeWords :: String -> String -> String
mergeWords [] x = x
mergeWords x [] = x
mergeWords x y = zipWith (\a b -> if a == '?' then b else if b == '?' then a else b) x y

mergeMaps :: Letters -> Letters -> Letters
mergeMaps = Map.unionWith (\(a, a1) (b, b1) -> if a > b then (a, Set.union a1 b1) else (b, Set.union a1 b1))

matchWords :: String -> String -> Bool
matchWords a = all (\(c1, c2) -> not (c1 /= '?' && c2 /= '?') || (c1 == c2)) . zip a

matchLetters :: Letters -> String -> Bool
matchLetters l w = all (\(n, c) -> Set.notMember n (get_places c)) $ zip [0..] w
    where 
        get_places c = maybe Set.empty snd (Map.lookup c l)

matchNotLetters :: Set Char -> String -> String -> Bool
matchNotLetters n w = or . zipWith (\a b -> a /= b && b `notElem` n) w

main :: IO ()
main = do
    args <- getArgs
    answers <- readWords $ if not (null args) then head args else "../answers.txt"
    go answers ("", Map.empty, Set.empty)
    where 
        go c (w, l, n) = do
            test <- (checkInput >=> processInput 0 ("", Map.empty, n)) <$> readInput
            case test of
                Right t -> do
                    let (w2, l2, n2) = t
                    if '?' `notElem` w2
                        then putStrLn $ "Answer: " <> w2
                        else do
                            let w3 = mergeWords w w2
                            let l3 = mergeMaps l2 l
                            mapM_ (putStr . (<>" ")) . filterAnswers (w3, l3, n2) . getAnswers c $ w3
                            putStrLn ""
                            hFlush stdout
                            go c (w3, l3, n2)
                Left err -> do
                    putStrLn err
                    go c (w, l, n)
