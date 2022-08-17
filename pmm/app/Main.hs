{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Directory
    ( doesDirectoryExist, getDirectoryContents )
import System.Environment ( getArgs )
import Control.Monad ( unless, forM_, forM, when, foldM)
import Data.List (isSuffixOf, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as B
import Data.Binary ( decode, encode )
import System.Random (Random(randomR, random), StdGen, initStdGen)
import Data.Tuple (swap)
import Options.Applicative
    ( auto,
      flag,
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      option,
      progDesc,
      short,
      strOption,
      value,
      execParser,
      helper,
      Parser )

type WordMap = Map Text (Map Text Int)

traverseDir :: WordMap -> FilePath -> (FilePath -> IO WordMap) -> IO WordMap
traverseDir m d f = do
  contents <- getDirectoryContents d
  fmap (foldl (M.unionWith (M.unionWith (+))) M.empty) $ forM contents $ \fn -> do
    let ful = d <> "/" <> fn
    isd <- doesDirectoryExist ful
    if isd && not ("." `isPrefixOf` fn)
      then traverseDir m ful f
      else if ".txt" `isSuffixOf` fn
        then f ful
        else return M.empty

convert2 :: WordMap -> [Text] -> WordMap
convert2 m [] = m
convert2 m [x] = m
convert2 m (x:xs) | "." `T.isSuffixOf` x || T.null (filterLetters x) = convert2 m xs
convert2 m (x:y:xs) | T.null (filterLetters y) = convert2 m (x:xs)
convert2 m (x:y:xs) = convert2 (M.insertWith (M.unionWith (+)) (filterLetters x) (M.singleton (filterLetters y) 1) m) xs

filterLetters :: Text -> Text
filterLetters = T.filter isAsciiLower . T.toCaseFold 

selectRandomWord :: StdGen -> Map Text Int -> (Text, StdGen)
selectRandomWord g m = let s = M.foldr (+) 0 m
                           w = map (\(k, v) -> (k, fromIntegral v / fromIntegral s)) $ M.assocs m
                           go [] _ _ = "."
                           go ((k, v):xs) p a = if p < a + v then k else go xs p (a + v)
                           (pp, g2) = random g :: (Double, StdGen)
                       in (go w pp 0.0, g2)

chain :: StdGen -> Int -> WordMap -> [Text] -> [Text]
chain _ 0 _ l = reverse l
chain g n m [] = let (v, g2) = randomR (0, length m) g in chain g2 (n-1) m [M.keys m !! v]
chain _ _ m l@(x:xs) | x `M.notMember` m = reverse (".":l)
chain g n m l@(x:xs) = let mi = m M.! x
                           (w, g2) = selectRandomWord g mi
                       in chain g2 (n-1) m (w:l)

learn :: FilePath -> FilePath -> IO ()
learn a o = do
  x <- traverseDir M.empty a $ \fn -> do
    things <- T.readFile fn
    return $ convert2 M.empty . concatMap T.words . T.lines $ things
  B.writeFile o $ encode x

data Mode = Generation | Learning deriving Show

data Options = Options {
  mode :: !Mode,
  input :: !String,
  output :: !String,
  number :: !Int,
  start :: !Text
}

options :: Parser Options
options = Options
  <$> flag Generation Learning (long "learning" <> short 'l' <> help "Mode")
  <*> strOption (long "input" <> short 'i' <> help "Input dir or file" <> metavar "INPUT" <> value ".")
  <*> strOption (long "output" <> short 'o' <> help "Output file" <> metavar "OUTPUT" <> value "output.dat")
  <*> option auto (long "number" <> short 'n' <> help "Number of words" <> metavar "NUMBER" <> value 100)
  <*> strOption (long "start" <> short 's' <> help "Start word" <> metavar "START" <> value "")

main :: IO ()
main = do
  args <- execParser opts
  case mode args of
    Learning -> learn (input args) (output args)
    Generation -> do
      g <- initStdGen
      x <- B.readFile (input args)
      let wm = decode x :: WordMap
      -- forM_ (M.assocs wm) $ \(k, v) -> do
      --   T.putStrLn k
      --   forM_ (M.assocs v) $ \(k1, v1) -> do
      --     T.putStrLn (" " <> k1 <> " " <> T.pack (show v1))
      mapM_ (T.putStr . (<>" ")) $ chain g (number args) wm ([start args | not (T.null (start args))])
      putStrLn ""
  where opts = info (helper <*> options)
          (fullDesc
           <> progDesc "Poor man's Markov chain"
           <> header "PMM" )