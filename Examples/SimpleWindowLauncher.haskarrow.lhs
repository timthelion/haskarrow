Compile this module with:
haskarrowPrecompiler -i SimpleWindowLauncher.haskarrow.lhs -o SimpleWindowLauncher.hs
ghc SimpleWindowLauncher.hs -threaded

>module Main where

All the interesting stuff is found in SimpleWindow.haskarrow.lhs

>import qualified SimpleWindow as SimpleWindow

All of the values in a haskarrow file are stored in a data declaration named "Values"  It is therefore important to import these files as qualified, because they will interfere with eachother otherwise.

>simpleWindow :: IO SimpleWindow.Values
>simpleWindow << do
> values <- SimpleWindow.initValues

The only purpose of this file, is to make the program not exit imediately.

> _ <- getChar
> return values
