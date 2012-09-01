>{-# LANGUAGE GADTs #-}
>module Language.Haskarrow.DependencyResolver
> (resolveDependencies) where
>import Language.Haskarrow.Types

>import Data.List
>import Data.Graph

>resolveDependencies ::
> [Value Unresolved Unresolved] ->
> Either
>  Error
>  [Value Resolved Unresolved]

>resolveDependencies
> values
>  =
> let

Don't be scared, we're just converting the Value's to tuples and back again, so we can use the graph lib to sort this "topoligically".

>  graphNodeTupleToValue 
>   ((influences,variety),name,depends)
>    = 
>   Value name (UnresolvedDepends depends) influences variety

>  (dependencyGraph, vertexToNode, _) =
>   graphFromEdges $
>    map
>     (\(Value
>         valueName
>         (UnresolvedDepends valueDepends)
>         valueInfluences
>         valueVariety) ->
>        ((valueInfluences,valueVariety),
>         valueName,
>         valueDepends))
>     values

>  sortedValues ::
>   [Value Unresolved Unresolved]
>  sortedValues =
>   reverse $
>    map
>     graphNodeTupleToValue $
>     map
>      vertexToNode
>      (topSort dependencyGraph)

↓ We must go through all of the depends, and check if they are resolvable.  If not, and this is an Evaluated value, this is an error.  If not, and this is a Static value, then we get rid of depends we cannot resolve. ↓

>  checkedDependsEither ::
>   Either
>    Error
>    [Value Resolved Unresolved]

>  checkedDependsEither =
>   sequence $
>    map
>     checkDepends
>     sortedValues

>  checkDepends ::
>   (Value Unresolved Unresolved) ->
>   Either
>     Error
>     (Value Resolved Unresolved)

>  checkDepends
>   value
>    =
>   let
>    myResolvedDepends
>      =
>     takeWhile
>      checkDepend
>      depends
>     where
>      depends =
>       unresolvedDepends
>        $ valueDepends
>           value
>   in
>   case evaluationType $ valueVariety value of
>    Evaluated ->
>     case length (unresolvedDepends $ valueDepends value) == length myResolvedDepends of
>      True ->
>       Right
>        value{valueDepends=ResolvedDepends (unresolvedDepends $ valueDepends value)}
>      False ->
>       Left
>        ("Error, could not resolve the dependencies of these\n" ++
>        "evaluated values:" ++
>        (concatMap
>         (\depend->
>           ' ':depend)
>         $ (unresolvedDepends $ valueDepends value) \\ myResolvedDepends) ++
>         "\nFrom the values:\n" ++
>         (unwords
>           $ map
>              valueName
>              values))
>    Static ->
>     Right
>      (value{
>       valueDepends
>         =
>        ResolvedDepends (unresolvedDepends $ valueDepends value)})

>  checkDepend ::
>   String ->
>   Bool

>  checkDepend
>   depend
>    =
>   any
>    (\value ->
>      depend == valueName value)
>    values

> in
>  checkedDependsEither
