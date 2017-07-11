module Main where

import ClecBit.XML

main :: IO ()
main = getSports >>= print
