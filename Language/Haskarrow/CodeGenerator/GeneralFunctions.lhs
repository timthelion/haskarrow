>{-# LANGUAGE GADTs #-}

>module Language.Haskarrow.CodeGenerator.GeneralFunctions(
> evaluationTypeOfInit,
> indentation,
> increaseIndentation,
> initHeader,
> initFooter,
> dependsAsParams,
> concatDepends,
> groupValuesByEvaluationType,
> EvaluationGroup(EvaluatedGroup,StaticGroup),

> optionalParametersEmptyCode,
> requiredParametersDataDeclaration,
> optionalParametersDataDeclaration,
> valuesDataDeclaration,
> mainCode
> )
> where

>import Language.Haskarrow.Types
>import Language.Haskarrow.CodeGenerator.Constants

>import Language.Haskell.Her.HaLay

>import Data.List
>import Data.Maybe

> -- | Are any values to be evaluated?  If not, no main statement can be created, and the type of initValues will not be IO.
>evaluationTypeOfInit ::
> [Value Resolved Resolved] ->
> EvaluationType

>evaluationTypeOfInit
> values
>  =
> case (any
>  (\Value
>      {valueVariety =
>        ValueVariety{
>         evaluationType=evaluated}}
>    -> evaluated == Evaluated)
>  values) of
>  True  -> Evaluated
>  False -> Static

> -- | Create an indent of the given number of spaces.
>indentation ::
> Indentation    ->
> String
>indentation
> (Indentation indent)
>  =
> indent

>increaseIndentation ::
> Indentation ->
> Indentation
>increaseIndentation
> (Indentation i)
>  =
> Indentation (i++(' '):[])

>initHeader ::
> [Value Resolved Resolved] ->
> Indentation     ->
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
> EvaluationType ->
> [Value Resolved Resolved] ->
> Indentation ->
> String

>initFooter
> evaluationTypeOfInit
> values
> topLevel
>  =
> (indentation (increaseIndentation topLevel)) ++
> (case evaluationTypeOfInit of
>   Evaluated -> "return $ "
>   Static    -> "in ") ++
> valuesDataConstructorName ++
> (concatMap
>  (\value ->
>    ' ':(valueName value) ++ internalInitializedValueSuffix)
>  values) ++ "\n"

↑ And return all of our values in a giant data constructor ↑

> -- | return a space seperated string listing the dependencies of the given values.
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
> mapMaybe
>  (\value ->
>   let
>    myParameter
>      =
>     isParameter
>      (valueVariety value)
>   in
>    case myParameter == parameterType of
>     True -> Just 
>               value
>     False -> Nothing)
>  values

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


>optionalParametersEmptyCode ::
> [Value Resolved Resolved] ->
> Indentation     ->
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
> Indentation     ->
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
> Indentation     ->
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
> Indentation ->
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
> Indentation     ->
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
> (indentation topLevel) ++
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
> Indentation ->
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
>  haveWeAnExitMethod
>    =
>   find
>    (\value ->
>       (valueName value)
>            ==
>          "exit")
>    values
> in
> if canMakeMain
>  then
>   (indentation topLevel) ++
>   "main :: IO ()\n"++
>   (indentation topLevel) ++
>   "main = do myValues <- initValues (" ++
>   requiredParametersDataConstructorName ++ ")("
>   ++ optionalParametersDataConstructorName ++ " " ++
>   (unwords $
>    take
>     numberOfOptionalParameters $
>     repeat "Nothing") ++
>   ") ;"++
>   (case haveWeAnExitMethod of
>    Just _ -> "(exitValue myValues);"
>    _ -> "")++
>   "return ()\n"
>  else "{-NoMain-}"

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
