
import Text.Yoda
import LinearAlgebra.MatrixParser
import LinearAlgebra.Smith

main = do putStrLn "Welcome, enter an integer matrix of the form [[a]]"
          s <- getLine
          let m = fst . head $ parse intMatrix s
          putStrLn ("Your input: \n" ++ show m)
          putStrLn "Smith normal-form: \n"
          putStrLn $ show (smith m)
          main
