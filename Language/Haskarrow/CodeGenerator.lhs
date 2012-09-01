>{-# LANGUAGE GADTs #-}

>module Language.Haskarrow.CodeGenerator
> (areAnyValuesToBeEvaluated         ,
>  optionalParametersEmptyCode       ,
>  requiredParametersDataDeclaration ,
>  optionalParametersDataDeclaration ,
>  valuesDataDeclaration             ,
>  generateConcurrentInit            ,
>  generateInit                      ,
>  mainCode)
> where

import Debug.Trace

>import Language.Haskarrow.Types
>import Language.Haskarrow.CodeGeneratorConstants

>import Language.Haskell.Her.HaLay

>import Data.List.Split
>import Data.List

>indentation ::
> Int    ->
> String
>indentation
> indent
>  =
> replicate indent ' '

>areAnyValuesToBeEvaluated ::
> [Value Resolved Resolved] ->
> Bool

>areAnyValuesToBeEvaluated
> values
>  =
> any
>  (\Value
>      {valueVariety =
>        ValueVariety{
>         evaluationType=evaluated}}
>    -> evaluated == Evaluated)
>  values

>initHeader ::
> [Value Resolved Resolved] ->
> Int     ->
> String

>initHeader
> values
> topLevel
>  =
> "-- | This action loads a haskarrow module.  It returns a giant data constructor with reccords for each value loaded.\n" ++ 
> "-- " ++ initValuesFunctionName ++ " " ++
>  requiredParametersDataConstructorName ++ " " ++
>  optionalParametersDataConstructorName ++ "\n" ++

↑ Everything up there is a comment ↑

 Ha!  I don't need to find the type. It'd be work now that I'm taking arguments.  But GHC will do it for me :D :D :D
 (replicate topLevel ' ') ++ "initValues :: IO Values\n"++

> (indentation topLevel) ++
> initValuesFunctionName ++ " (" ++
> requiredParametersDataConstructorName ++ " " ++
> (concatMap
>  (\value ->
>   (valueName value) ++
>   valueSuffix ++
>   " ")
>  $ requiredParameters values) ++
> ") (" ++
> optionalParametersDataConstructorName ++ " " ++
> (codifiedParameters $ optionalParameters values) ++ ") ="

↑ Insert the parameters ↑

>initFooter ::
> Bool ->
> [Value Resolved Resolved] ->
> Int ->
> String

>initFooter
> evaluated
> values
> topLevel
>  =
> (indentation (topLevel+1)) ++
> (if evaluated
>   then "return $ "
>   else "in ") ++
> valuesDataConstructorName ++
> (concatMap
>  (\value ->
>    ' ':(valueName value) ++ internalInitializedValueSuffix)
>  values) ++ "\n"

↑ And return all of our values in a giant data constructor ↑


>generateConcurrentInit ::
> Bool    ->
> -- ^ Evaluated? Is this to be an IO typed function?
> [Value Resolved Resolved] ->
> -- ^ The values to be inintialized.
> Int     ->
> -- ^ The level of indentation in number of spaces.  No tabs please :P
> String
> -- ^ The source code we generated.

↓ We IO concurrency makes no sense without IO ↓

>generateConcurrentInit
> False
> values
> topLevel
>  =
> generateInit
>  False
>  values
>  topLevel

>generateConcurrentInit
> _
> values
> topLevel
>   =

  let
   values =
    filter
     (not . isCertainlyParametric)
     values'
  in

>  (initHeader 
>    values
>    topLevel) ++
>  "do\n" ++
>   (concatMap
>       (declareMVar
>         (topLevel+1))
>       values) ++
>  (concatMap
>   (concurrentValueGroupInit
>    (topLevel+1))
>   $ groupValuesByEvaluationType
>      values) ++
>  (concatMap
>    (\depend ->
>       (indentation
>         (topLevel+1))++
>       (concurrentDependRead
>         depend)++
>       "\n")
>    (map
>      valueName
>      values))++
>  (initFooter
>    True
>    values
>    topLevel)

>declareMVar ::
> Int ->
> (Value Resolved Resolved) ->
> String

>declareMVar
> indent
> Value{
>  valueName=name}
>  =
> (indentation indent) ++
> name ++
> "MVar" ++
> internalInitializedValueSuffix ++
> " <- newEmptyMVar\n"

>concurrentDependencyMVarReadingHeader ::
> Int ->
> [String] ->
> String

>concurrentDependencyMVarReadingHeader
> indent
> depends
>  =
> (indentation indent) ++
> "_<-forkIO $ do{\n" ++
> (concatMap
>   (\depend ->
>      (indentation
>        (indent+1)) ++
>      (concurrentDependRead
>        depend)++
>      ";\n")
>   depends)

>concatDepends ::
> [Value Resolved Resolved] ->
> [String]
>concatDepends
> values
>  =
> filter
>  (\depend->
>    not
>     $ any
>        (\value ->
>           depend == valueName value)
>        values)
>  (nub
>   $ concatMap
>      (resolvedDepends . valueDepends)
>      values)

>staticValueInitCode ::
> (Value Resolved Resolved) ->
> String

>staticValueInitCode
> Value{
>   valueName=name,
>   valueDepends=(ResolvedDepends depends),
>   valueVariety=ValueVariety{
>     isParameter=MaybeParametricValue}}
>   =
> name ++
> internalInitializedValueSuffix ++
> " = (case " ++
> name ++
> parameterSuffix ++
> " of Nothing -> " ++
> name ++
> valueSuffix ++
> " " ++
> (dependsAsParams
>  depends) ++
> " ; Just parameter_'_ -> parameter_'_)"

>staticValueInitCode
> Value{
>   valueName=name,
>   valueDepends=(ResolvedDepends depends)}
>   =
> name ++
> internalInitializedValueSuffix ++
> " = " ++
> name ++
> valueSuffix ++
> " " ++
> (dependsAsParams
>  depends)

>concurrentValueGroupInit ::
> Int ->
> EvaluationGroup ->
> String

>concurrentValueGroupInit
> indent
> (StaticGroup values)
>  =
> (concurrentDependencyMVarReadingHeader
>   indent
>   (concatDepends
>      values)) ++
> (indentation
>   (indent + 1)) ++
> "let\n" ++
> (concatMap
>   (\value ->
>     (indentation
>       (indent + 2)) ++
>     (staticValueInitCode value) ++
>     ";\n")
>   values) ++
> (indentation
>   (indent + 1)) ++
> "in do {\n" ++
> (concatMap
>  (\value ->
>     (indentation
>       (indent + 2)) ++
>     ((mvarPutSequence . valueName)
>        value) ++
>     ";\n")
>  values)++
> (indentation
>   (indent + 1)) ++
> "}}\n"

>concurrentValueGroupInit
> indent
> (EvaluatedGroup values)
>  =
> concatMap
>  (concurrentEvaluatedValueInit
>    indent)
>  values

>concurrentEvaluatedValueInit ::
> Int ->
> (Value Resolved Resolved) ->
> String

>concurrentEvaluatedValueInit
> indent
> Value{
>  valueName=name,
>  valueDepends=(ResolvedDepends depends),
>  valueVariety=ValueVariety{
>    isParameter=parameter}}
>  =
> (concurrentDependencyMVarReadingHeader
>   indent
>   depends) ++
> (indentation
>   (indent+1)) ++
> name ++
> internalInitializedValueSuffix ++
> " <- " ++
> (case parameter of
>   InternalValue ->
>    name ++
>    valueSuffix ++
>    " " ++
>    (dependsAsParams
>      depends)
>   MaybeParametricValue ->
>    " (case " ++
>    name ++
>    parameterSuffix ++
>    " of Nothing -> " ++
>    name ++
>    valueSuffix ++
>    " " ++
>    (dependsAsParams
>      depends) ++
>    " ; Just parameter_'_ -> parameter_'_)") ++
> " ;\n" ++
> (indentation
>   (indent + 1)) ++
> (mvarPutSequence
>  name)++
> "}\n"

>mvarPutSequence ::
> String ->
> String

>mvarPutSequence
> name
>  =
> "putMVar " ++
> name ++
> "MVar" ++
> internalInitializedValueSuffix ++
> " " ++
> name ++
> internalInitializedValueSuffix

>concurrentDependRead ::
> String ->
> String

>concurrentDependRead
> depend
>  =
> depend ++
> internalInitializedValueSuffix ++
> " <- readMVar " ++
> depend ++
> "MVar" ++
> internalInitializedValueSuffix

>generateInit ::
> Bool    ->
> -- ^ Evaluated? Is this to be an IO typed function?
> [Value Resolved Resolved] ->
> -- ^ The values to be inintialized.
> Int     ->
> -- ^ The level of indentation in number of spaces.  No tabs please :P
> String
> -- ^ The source code we generated.
>
>generateInit
> evaluated
> values
> topLevel
>  =
> (initHeader
>  values
>  topLevel) ++

> (if evaluated
>  then " do\n"
>  else "\n") ++

> (concatMap
>  (\group ->
>    valueGroupCode group topLevel) $
>  groupValuesByEvaluationType
>   $ filter
>     (not . isCertainlyParametric)
>     values) ++

↑ Actually create the body of our function. ↑

> (initFooter
>   evaluated
>   values
>   topLevel)

>codifiedParameters ::
> [Value Resolved Resolved] ->
> String
>codifiedParameters
> values =
> unwords $
> map
>  (\name ->
>   name ++ "Parameter")
>  $ map 
>     valueName
>     values
 
>optionalParameters ::
> [Value Resolved Resolved] ->
> [Value Resolved Resolved]

>optionalParameters values =
> parameters
>  MaybeParametricValue
>  values

>requiredParameters ::
> [Value Resolved Resolved] ->
> [Value Resolved Resolved]

>requiredParameters values =
> parameters CertainlyParametricValue values

>parameters ::
> IsParameter ->
> [Value Resolved Resolved]     ->
> [Value Resolved Resolved]

>parameters
> parameterType
> values
>  =
> filter
>  (\value ->
>   let
>    amIParameter
>      =
>     isParameter
>      (valueVariety value)
>   in
>    amIParameter == parameterType)
>  values

>data EvaluationGroup =
> EvaluatedGroup [Value Resolved Resolved] |
> StaticGroup    [Value Resolved Resolved] 

>groupValuesByEvaluationType ::
> [Value Resolved Resolved] ->
> [EvaluationGroup]

>groupValuesByEvaluationType
> (value:values)
>  =
> case valueVariety value of
>  ValueVariety{evaluationType = Evaluated}-> 
>   gatherEvaluatedGroup
>    values
>    [value]
>  ValueVariety{evaluationType = Static} ->
>   gatherStaticGroup
>    values
>    [value]

>groupValuesByEvaluationType
> []
>  =
> []

>gatherEvaluatedGroup ::
> [Value Resolved Resolved] ->
> -- ^ Unsorted values.
> [Value Resolved Resolved] ->
> -- ^ Evaluated Values
> [EvaluationGroup]

>gatherEvaluatedGroup
> (value:values)
> inGroupAlready
>  =
> case valueVariety value of
>  ValueVariety{evaluationType = Evaluated} ->
>    gatherEvaluatedGroup
>     values
>     (inGroupAlready ++ [value])
>  ValueVariety{evaluationType = Static} ->
>    (EvaluatedGroup
>      inGroupAlready):
>       gatherStaticGroup
>        values
>        [value]

>gatherEvaluatedGroup
> []
> inGroupAlready
>  =
> [EvaluatedGroup inGroupAlready]

>gatherStaticGroup ::
> [Value Resolved Resolved] ->
> -- ^ Unsorted values.
> [Value Resolved Resolved] ->
> -- ^ Static Values
> [EvaluationGroup]

>gatherStaticGroup
> (value:values)
> inGroupAlready
>  =
> case valueVariety value of
>  ValueVariety{evaluationType=Static} ->
>    gatherStaticGroup
>     values
>     (inGroupAlready ++ [value])
>  ValueVariety{evaluationType=Evaluated} ->
>    (StaticGroup
>      inGroupAlready):
>       gatherEvaluatedGroup
>        values
>        [value]

>gatherStaticGroup
> []
> inGroupAlready
>  =
> [StaticGroup inGroupAlready]

>valueGroupCode ::
> EvaluationGroup ->
> Int ->
> -- ^ Indentation level.
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
>   (indent+1)) ++
> "let\n" ++
> (concatMap
>  (\value ->
>    initValueCode
>     value
>     (indent+1))
>  values) ++
> (indentation
>   indent) ++ "\n"

>dependsAsParams ::
> [String] ->
> String

>dependsAsParams
> depends
>  =
> concatMap
>  (\depend ->
>    ' ':depend ++
>        internalInitializedValueSuffix ++ " ")
>  depends

>initValueCode ::
> (Value Resolved Resolved) ->
> Int   ->
> String

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
> (indentation (topLevel+1)) ++
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
> (indentation (topLevel+1)) ++
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
> (indentation (topLevel+1)) ++ name ++ operator ++
> (name ++ "Parameter") ++
> (concatMap
>  (\depend ->
>    ' ':depend ++ " ")
>  depends) ++ "\n"

>optionalParametersEmptyCode ::
> [Value Resolved Resolved] ->
> Int     ->
> String

>optionalParametersEmptyCode
> values
> indent
>  =
> indentation indent ++
> optionalParametersEmpty ++ " = (" ++
> optionalParametersDataConstructorName ++ " " ++
> (unwords $
>   replicate
>    (length
>      $ optionalParameters
>         values)
>    "Nothing")
> ++ ")\n" 

>requiredParametersDataDeclaration ::
> [Value Resolved Resolved] ->
> Int     ->
> String

>requiredParametersDataDeclaration
> values
> indent
>  =
> dataTypeDeclaration
>  (requiredParameters values)
>  indent
>  requiredParametersDataConstructorName
>  parameterSuffix
 
>optionalParametersDataDeclaration ::
> [Value Resolved Resolved] ->
> Int     ->
> String
>optionalParametersDataDeclaration
> values
> indent
>  =
> dataTypeDeclaration
>  (optionalParameters values)
>  indent
>  optionalParametersDataConstructorName
>  parameterSuffix 

>valuesDataDeclaration ::
> [Value Resolved Resolved] ->
> Int ->
> String
>valuesDataDeclaration
> values
> indent
>  =
> dataTypeDeclaration
>  values
>  indent
>  valuesDataConstructorName
>  valueRecordSuffix

>dataTypeDeclaration ::
> [Value Resolved Resolved] ->
> -- ^ Values to be represented.
> Int     ->
> -- ^ Indentation level of the top level block.
> String  ->
> -- ^ Name of type and constructor.
> String  ->
> -- ^ Suffix of record sellectors.
> String
> -- ^ Source code of new constructor.
> 
>dataTypeDeclaration
> values
> topLevel
> name
> suffix
>  =
> (replicate topLevel ' ') ++
> "data " ++
> name ++ " " ++
> (concatMap
>   (\value ->
>    " "++
>    (valueName value)++
>    internalValueTypeSuffix)
>   values) ++
> " = " ++
> name ++" {\n" ++

> (tail
>  $(concatMap
>   (\value ->
>    ',' : ((valueName value) ++
>           suffix    ++
>           "::"      ++
>           (valueName value) ++
>           internalValueTypeSuffix ++"\n"))
>   values)++ " }\n")

>mainCode ::
> Int ->
> [Value Resolved Resolved] ->
> String

>mainCode
> topLevel
> values
>  = 
> let
>  numberOfRequiredParameters
>    =
>   length
>    $ requiredParameters
>       values
>  numberOfOptionalParameters =
>   length
>    $ optionalParameters
>       values

↓ A main statement is only generated if there are Evaluated values to be run, and there are no non-optional parameteres to initValues... Here we only need to check if there are non optional parameters, we checked for the need to evaluate above. ↓

>  canMakeMain =
>   null
>    $ requiredParameters
>       values
> in
> if canMakeMain
>  then
>   (replicate topLevel ' ') ++
>   "main :: IO ()\n"++
>   (replicate topLevel ' ') ++
>   "main = do myValues <- initValues (" ++
>   requiredParametersDataConstructorName ++ ")("
>   ++ optionalParametersDataConstructorName ++ " " ++
>   (unwords $
>    take
>     numberOfOptionalParameters $
>     repeat "Nothing") ++
>   ") ; (exitValue myValues) ; return ()\n"
>  else "{-NoMain-}"
