module Main where

import           Spread.Utils
import           Utils.Forward

main :: IO ()
main = run 1

run :: Int -> IO ()
run num = createDestinations >>= (\destis -> writeOutIO destis num) >> return ()
