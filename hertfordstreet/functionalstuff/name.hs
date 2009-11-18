module Main
    where

import IO

main = do
  hSetBuffering stdin LineBuffering
  putStrLn "Please enter your name:"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you?")
  putStrLn ("I don't believe this is happening!")
  response <- getLine
  putStrLn ("You said \"" ++ response ++ "\", how the hell does this work?")
                                          