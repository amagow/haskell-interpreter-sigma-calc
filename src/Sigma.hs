module Sigma where

import System.IO

import Parser
import Declare
import Source
import Target

sigma :: IO ()
sigma = do
          putStrLn "This is the Command Line Tool for Sigma"
          printMenu
          line <- getLine
          case line of
            "1" -> evaluateExpression
            "2" -> evaluateExpressionFile
            "3" -> parseToExpression
            "4" -> parseToSigmaExpression
            "5" -> return () 
            _   -> do 
              putStrLn "Please choose from options above"
              putStrLn ""
              sigma

printMenu :: IO ()
printMenu = do
    putStrLn "Choose from the following features: (1-5)"
    putStrLn "1. Evaluate code from Input"
    putStrLn "2. Evaluate code from File"
    putStrLn "3. Parse code to Exp"
    putStrLn "4. Parse code to SigmaTerm"
    putStrLn "5. Quit"
    putStr "Enter your choice: "

evaluateExpression :: IO ()
evaluateExpression = do
  putStrLn "Input Expression (Quit to exit)"
  line <- getLine
  if line == "Quit" then return () else do
    case parseExpr line of
      Right exp -> print(executeM (translate exp []))
      Left _    -> error "Error while parsing"
    evaluateExpression

evaluateExpressionFile :: IO ()
evaluateExpressionFile = do
  putStrLn "Input File (Quit to exit)"
  line <- getLine
  if line == "Quit" then return () else do
    file <- readFile line
    case parseExpr file of
      Right exp -> print(executeM (translate exp []))
      Left _    -> error "Error while parsing"
    evaluateExpression

parseToExpression :: IO ()
parseToExpression = do
  putStrLn "Input Expression to parse (Quit to exit)"
  line <- getLine
  if line == "Quit" then return () else do
    case parseExpr line of
      Right exp -> print exp
      Left _    -> error "Error while parsing"
    evaluateExpression

parseToSigmaExpression :: IO ()
parseToSigmaExpression = do
  putStrLn "Input Expression to parse (Quit to exit)"
  line <- getLine
  if line == "Quit" then return () else do
    case parseExpr line of
      Right exp -> print (translate exp [])
      Left _    -> error "Error while parsing"
    evaluateExpression