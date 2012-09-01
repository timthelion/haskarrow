>module HaskarrowPrecompile where

>import Language.Haskarrow.DependencyResolver
>import Language.Haskarrow.Types
>import Language.Haskarrow.CodeGenerator
>import Language.Haskarrow.Parser
>import Language.Haskarrow.ResolveLoudSources

import Debug.Trace

require her-lexer >= 0.1 from cabal

import Language.Haskell.Her.HaLay
import Language.Haskell.Her.FromClutterToLines

require cpphs from cabal

import Language.Preprocessor.Unlit

require split from cabal

import Data.List.Split
import Data.List

import Data.Graph

↓ Returns either HaskellCode OR AnError ↓

>precompile ::
> Bool     ->
> String   ->
> FilePath ->
> Either
>  Error
>  String

>precompile
> concurrent
> origionalSource
> fileName
>  =
> let
>  eitherValuesAndFunctionsOrAnError =
>   expandFunctions origionalSource fileName
> in
>  do
>    (values,functions,indent)
>     <- eitherValuesAndFunctionsOrAnError
>    valuesRU <- resolveDependencies values
>    valuesRR <- resolveLoudSources valuesRU
>    let
>     evaluated
>       =
>      areAnyValuesToBeEvaluated valuesRR
>    Right ("{- AUTOGENERATED BY HASKARROW -}"++
>           functions ++ "\n" ++
>           (optionalParametersEmptyCode valuesRR indent) ++
>           (requiredParametersDataDeclaration valuesRR indent) ++
>           (optionalParametersDataDeclaration valuesRR indent) ++
>           (valuesDataDeclaration valuesRR indent) ++
>           (if concurrent
>             then
>              (generateConcurrentInit evaluated valuesRR indent)
>             else
>              (generateInit evaluated valuesRR indent)) ++
>           (if evaluated
>             then
>              (mainCode indent valuesRR)
>             else "{-NoMain, nothing to do.-}"))
