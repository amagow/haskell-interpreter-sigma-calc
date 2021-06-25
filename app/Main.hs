module Main where

import System.Environment   
import System.Directory
import Data.List  
import System.IO

import Parser
import Declare
import Target
import Source
import Sigma

main :: IO ()
main = do
    args <- getArgs
    dir <- getCurrentDirectory
    let fileName = dir ++ "/test/" ++ head args
    handle <- openFile fileName ReadMode  
    contents <- hGetContents handle
    let Right v = parseExpr contents
    let ans = execute (translate v [])
    print ans
