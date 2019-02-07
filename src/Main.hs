module Main where

import Text.Yoda
import Data.Matrix
import LinearAlgebra.MatrixParser
import LinearAlgebra.Smith
import System.Environment
import System.Exit


-- TODO change getLine to getArgs and concatenate all args into one, the rest is the same

main = do ls <- getArgs
          let l = concat ls
          let m = parseMatrix l
          putStrLn "Input:"
          output m
          let s = m >>= maybeSmith
          putStrLn "Corresponding Smith normal-form:"
          output s

output :: (Show a) =>  Maybe a -> IO ()
output Nothing  = invalidArgs
output (Just a) = print a

parseMatrix :: String -> Maybe (Matrix Integer)
parseMatrix [] = Nothing
parseMatrix xs = let m = parse intMatrix xs in
                 case m of
                   []         -> Nothing
                   ((m,s):xs) -> Just m

maybeSmith :: Matrix Integer -> Maybe (Matrix Integer)
maybeSmith m = return $ smith m


invalidArgs = putStrLn msg >> exit where
    msg = "Error: Parse error on input. \n\n" ++
          "Command should be of the form: \n" ++
          ">>> smith [[1,2,3],[4,5,6],[7,8,9]]"-- TODO, write this up properly for usability

exit = exitWith ExitSuccess
