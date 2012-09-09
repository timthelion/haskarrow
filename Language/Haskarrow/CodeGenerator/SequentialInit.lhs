>{-# LANGUAGE GADTs #-}

>module Language.Haskarrow.CodeGenerator.SequentialInit
> (generateSequentialInit)
> where

>import Language.Haskarrow.Types
>import Language.Haskarrow.CodeGenerator.Constants
>import Language.Haskarrow.CodeGenerator.GeneralFunctions

>import Language.Haskell.Her.HaLay

>import Data.List.Split
>import Data.List

>generateSequentialInit ::
> EvaluationType    ->
> -- ^ Evaluated? Is this to be an IO typed function?
> [Value Resolved Resolved] ->
> -- ^ The values to be inintialized.
> Indentation     ->
> -- ^ The level of indentation in number of spaces.  No tabs please :P
> String
> -- ^ The source code we generated.
>
>generateSequentialInit
> initEvaluationType
> values
> topLevel
>  =
> (initHeader
>  values
>  topLevel) ++

> (case initEvaluationType of
>   Evaluated -> " do\n"
>   Static    -> "\n") ++

> (concatMap
>  (\group ->
>    valueGroupCode group topLevel) $
>  groupValuesByEvaluationType
>   $ filter
>     (not . isCertainlyParametric)
>     values) ++

↑ Actually create the body of our function. ↑

> (initFooter
>   initEvaluationType
>   values
>   topLevel)

>valueGroupCode ::
> EvaluationGroup ->
> Indentation ->
> String

>valueGroupCode
> (EvaluatedGroup values)
> indent
>  =
> "-- Begginning of Evaluated group\n" ++
> (concatMap
>  (\value ->
>    initValueCode
>     value
>     indent)
>  values) ++
>  "-- End of evaluated group\n" 

>valueGroupCode
> (StaticGroup values)
> indent
>  =
> (indentation 
>   (increaseIndentation indent)) ++
> "let\n" ++
> (concatMap
>  (\value ->
>    initValueCode
>     value
>     (increaseIndentation indent))
>  values) ++
> (indentation
>   indent) ++ "\n"

>initValueCode ::
> (Value Resolved Resolved) ->
> Indentation          ->
> String

> -- | The code for initializing/binding a single value.
>initValueCode
> Value{
>   valueName=name,
>   valueDepends=(ResolvedDepends depends),
>   valueVariety=ValueVariety{
>     evaluationType=evaluationType,
>     isParameter=InternalValue}}
> topLevel
>  =
> let
>  operator =
>   case evaluationType of
>    Evaluated -> " <- "
>    Static    -> " = "
> in
> (indentation (increaseIndentation topLevel)) ++
> name ++ internalInitializedValueSuffix ++
> operator ++
> (name++valueSuffix) ++
> (dependsAsParams
>   depends) ++ "\n"

>initValueCode
> Value{
>  valueName=name,
>  valueDepends=(ResolvedDepends depends),
>  valueVariety=ValueVariety{
>    evaluationType=evaluationType,
>    isParameter=MaybeParametricValue}}
> topLevel
>  =
> let
>  operator =
>   case evaluationType of
>    Evaluated -> " <- "
>    Static    -> " = "
> in
> (indentation (increaseIndentation topLevel)) ++
> name ++
> internalInitializedValueSuffix ++
> operator ++ "case " ++
>  name ++ parameterSuffix ++
>  " of Nothing -> " ++
>   name ++ valueSuffix ++
>   (concatMap
>    (\depend ->
>      ' ':depend ++ " ")
>    depends) ++
>   "; Just parameter_'_ -> parameter_'_ " ++
>   (concatMap
>    (\depend ->
>      ' ':depend ++ " ")
>    depends) ++ "\n"

>initValueCode
> Value{
>   valueName=name,
>   valueDepends=(ResolvedDepends depends),
>   valueVariety=ValueVariety{
>    evaluationType=evaluationType,
>    isParameter=CertainlyParametricValue}}
> topLevel
>  =
> let
>  operator =
>   case evaluationType of
>    Evaluated -> " <- "
>    Static    -> " = "
> in
> (indentation (increaseIndentation topLevel)) ++ name ++ operator ++
> (name ++ "Parameter") ++
> (concatMap
>  (\depend ->
>    ' ':depend ++ " ")
>  depends) ++ "\n"
