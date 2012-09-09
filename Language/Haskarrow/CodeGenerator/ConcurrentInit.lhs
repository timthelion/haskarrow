>{-# LANGUAGE GADTs #-}

>module Language.Haskarrow.CodeGenerator.ConcurrentInit
> (generateConcurrentInit)
> where
>import Language.Haskarrow.Types
>import Language.Haskarrow.CodeGenerator.Constants
>import Language.Haskarrow.CodeGenerator.GeneralFunctions
>import Language.Haskarrow.CodeGenerator.SequentialInit

>import Language.Haskell.Her.HaLay

>generateConcurrentInit ::
> EvaluationType    ->
> -- ^ Evaluated? Is this to be an IO typed function?
> [Value Resolved Resolved] ->
> -- ^ The values to be inintialized.
> Indentation     ->
> -- ^ The level of indentation in number of spaces.  No tabs please :P
> String
> -- ^ The source code we generated.

↓ We IO concurrency makes no sense without IO ↓

>generateConcurrentInit
> Static
> values
> topLevel
>  =
> generateSequentialInit
>  Static
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
>         (increaseIndentation topLevel))
>       values) ++
>  (concatMap
>   (concurrentValueGroupInit
>    (increaseIndentation topLevel))
>   $ groupValuesByEvaluationType
>      values) ++
>  (concatMap
>    (\depend ->
>       (indentation
>         (increaseIndentation topLevel))++
>       (concurrentDependRead
>         depend)++
>       "\n")
>    (map
>      valueName
>      values))++
>  (initFooter
>    Evaluated
>    values
>    topLevel)

>declareMVar ::
> Indentation ->
> (Value Resolved Resolved) ->
> String

>declareMVar
> indent
> Value{
>  valueName=name}
>  =
> (indentation indent) ++
> name ++
> concurrentInitMVarSuffix ++
> internalInitializedValueSuffix ++
> " <- newEmptyMVar\n"

>concurrentDependencyMVarReadingHeader ::
> Indentation ->
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
>        (increaseIndentation indent)) ++
>      (concurrentDependRead
>        depend)++
>      ";\n")
>   depends)

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
> Indentation ->
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
>   (increaseIndentation indent)) ++
> "let\n" ++
> (concatMap
>   (\value ->
>     (indentation
>       ((increaseIndentation . increaseIndentation) indent)) ++
>     (staticValueInitCode value) ++
>     ";\n")
>   values) ++
> (indentation
>   (increaseIndentation indent)) ++
> "in do {\n" ++
> (concatMap
>  (\value ->
>     (indentation
>       ((increaseIndentation.increaseIndentation) indent)) ++
>     ((mvarPutSequence . valueName)
>        value) ++
>     ";\n")
>  values)++
> (indentation
>   (increaseIndentation indent)) ++
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
> Indentation ->
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
>   (increaseIndentation indent)) ++
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
>   (increaseIndentation indent)) ++
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
> concurrentInitMVarSuffix ++
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
> concurrentInitMVarSuffix ++
> internalInitializedValueSuffix
