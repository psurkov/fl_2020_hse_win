module Main where

import Parser


runParser :: String -> IO ()
runParser str =
  case parseString str of
    Left err -> print err
    Right r -> print r

main :: IO ()
main = do
  putStrLn ""

  runParser "13"
  runParser "42"
  runParser "007"
  runParser "a"
  runParser "(a+13)*42"
  runParser "a+13*42"
  runParser "1^2^3^4"
  runParser "a+2^3*4"
