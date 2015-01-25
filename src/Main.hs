module Main where

import Control.Monad (when)
import qualified Data.Map as M

type Freq = M.Map Char Int

measure :: String -> Freq
measure = foldl (\m ch -> M.insertWith (+) ch 1 m) M.empty

(.<.) :: Freq -> Freq -> Bool
x .<. y = and [n <= M.findWithDefault 0 ch y | (ch, n) <- M.toList x]

elephantPig :: Freq
elephantPig = measure "elephantPig"

main :: IO ()
main = getContents >>= mapM_ test . lines
  where
    test s = when (measure s .<. elephantPig) $ putStrLn s
