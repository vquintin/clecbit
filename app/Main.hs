module Main where

import Lib

main :: IO ()
main = getSports >>= print
