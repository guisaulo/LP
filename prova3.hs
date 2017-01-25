
module Chapter18 where

import Prelude hiding (lookup)

import IO       -- for isEOF (see note below, aslo)

isEOF = hugsIsEOF

while :: IO Bool -> IO () -> IO ()

while test oper
  = do res <- test
       if res then do oper
                      while test oper
              else return ()

copyInputToOutput :: IO ()
copyInputToOutput
  = while (do res <- isEOF
              return (not res))
          (do line <- getLine
              putStrLn line)