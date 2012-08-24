require cmdargs

>module Main where
>import System.IO
>import System.Console.CmdArgs.Implicit
>import HaskarrowPrecompileToFile

>data HaskarrowPrecompilerOptions = HaskarrowPrecompilerOptions{
> inputFile :: FilePath,
> outputFile:: FilePath,
> version   :: Bool}deriving(Data,Typeable)

>haskarrowPrecompiler = HaskarrowPrecompilerOptions{
> inputFile  =
>  def &= typFile &= name "i" &= help "The haskarrow file to be pre-compiled.",

> outputFile =
>  def &= typFile &= name "o" &= help "The outputted .hs file.",

> Main.version =
>  def &= name "v" &= help "Print out haskarrow's version."

> } &= help "Precompile a haskarrow file to .hs"

>main :: IO()
>main = do 
> args  <- cmdArgs haskarrowPrecompiler
> if not ((null (inputFile args)) || (null (outputFile args)))
>  then preCompileToFile
>   (inputFile args)
>   (outputFile args)
>  else (

If one file is given but not the other print an error...

>   if not ((null (inputFile args)) && (null (outputFile args)))
>    then print "Both an input AND output file must be specified to precompile."
>    else return ())

> if Main.version args then print versionString else return()

>versionString = "Version: 0"
