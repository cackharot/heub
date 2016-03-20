module Lib
    ( someFunc
    ) where

{- |
    An someFunc prints "someFunc" on the console when run

    >>> someFunc
    someFunc
-}
someFunc :: IO ()
someFunc = putStrLn "someFunc"
