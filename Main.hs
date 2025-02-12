module Main where

import System.Environment (getArgs)
import Parser (parseComm)

-- Modificar este import para usar diferentes evaluadores
import EvalMonadico2
---------------------------------------------------------

main :: IO ()
main = do arg:_ <- getArgs
          run arg

run :: [Char] -> IO ()
run ifile = do
    s <- readFile ifile
    case parseComm ifile s of
      Left error -> print error
      Right t    -> do 
                      result <- eval t
                      print result
                    --print t  -- si descomento esta y comento las 2 de arriba habilito mostar el AST
          
