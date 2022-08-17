module Main where

import System.Directory
    ( doesDirectoryExist, getDirectoryContents )
import System.Environment ( getArgs )
import Control.Monad ( unless, forM, when, foldM)
import Data.List (isSuffixOf, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as B
import Data.Binary ( decode )

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
convert2 m (x:y:xs) = convert2 (M.insertWith (M.unionWith (+)) (filterLetters x) (M.singleton y 1) m) xs 
  where filterLetters = T.filter isAlphaNum

-- chain :: Int -> WordMap -> [Text] 
-- chain 0 _ l = reverse l
-- chain n m [] =  

main :: IO ()
main = do
  x <- B.readFile "output.dat"
  print . length $ (decode x :: WordMap)
  -- args <- getArgs
  -- unless (null args) $ do
  --   x <- traverseDir M.empty (head args) $ \fn -> do
  --           things <- T.readFile fn
  --           return $ convert2 M.empty . concatMap T.words . T.lines $ things
  --   B.writeFile "output.dat" $ encode x