module Main where

-- This imports `Parser.hs`, `Primitives.hs` and `Types.hs` because Primitives imports Parser and Parser imports Types
import Primitives

-- Needed for the function `clearScreen`
import System.Console.ANSI
import System.Environment

-- Just a general heading that happens at REPL start-up, and is easier to make a function here than to put this all in `main`
welcomeScreen :: IO ()
welcomeScreen = do
    clearScreen
    putStrLn ",-----.                  ,-----.      ,-----.      ,--.       ,------."
    putStrLn "|  |) /_     ,--.,--.    |  |) /_     |  |) /_     |  |       |  .---'"
    putStrLn "|  .-.  \\    |  ||  |    |  .-.  \\    |  .-.  \\    |  |       |  `--,"
    putStrLn "|  '--' /    '  ''  '    |  '--' /    |  '--' /    |  '--.    |  `---."
    putStrLn "`------'      `----'     `------'     `------'     `-----'    `------'"
    putStrLn ""
    putStrLn "A LISP written in 100% Haskell... Didn't expect that one, did you?"
    putStrLn ""
    putStrLn "Copyright (c) Ninjacop123 - 2018"
    putStrLn ""
    putStrLn "Type :q to exit the REPL"
    putStrLn "Type :d to view the documentation"
    putStrLn ""
    putStrLn ""
    putStrLn "" 


-- The, main, the myth, the legend
main :: IO ()
main = do
   args <- getArgs
   if null args then do
    welcomeScreen
    runRepl 
    else runOne $ args




-- functions are weird coming from Lisp