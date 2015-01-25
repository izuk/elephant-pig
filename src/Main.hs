module Main where

import Control.Monad (when)
import Data.Char (toLower)
import qualified Data.Map as M
import System.Environment (getArgs)

type Freq = M.Map Char Int

measure :: String -> Freq
measure = foldl (\m ch -> M.insertWith (+) ch 1 m) M.empty . downcase

(.<.) :: Freq -> Freq -> Bool
x .<. y = and [n <= M.findWithDefault 0 ch y | (ch, n) <- M.toList x]

downcase :: String -> String
downcase = map toLower

main :: IO ()
main = do
  [word] <- getArgs
  let elephantPig = measure word
  getContents >>= mapM_ (test elephantPig) . lines
  where
    test f s = when (measure s .<. f) $ putStrLn s
