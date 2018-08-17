module Main where

import Parser
import System.Console.ANSI
import System.Environment

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


main :: IO ()
main = do
   args <- getArgs
   if null args then do
    welcomeScreen
    runRepl 
    else runOne $ args




-- functions are weird