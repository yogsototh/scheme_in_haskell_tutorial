module Main where
import System.Environment

main :: IO ()
main = do
    putStrLn "Enter your name: "
    name <- getLine
    putStrLn ("Hello, " ++ name)

