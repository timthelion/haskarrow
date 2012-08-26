>module Language.Haskarrow.DependencyResolver where
>import Language.Haskarrow.Types

>import Data.List
>import Data.Graph

>resolveDependencies ::
> [Value] ->
> Either
>  Error
>  [Value]

>resolveDependencies
> values
>  =
> let

Don't be scared, we're just converting the Value's to tuples and back again, so we can use the graph lib to sort this "topoligically".

>  graphNodeTupleToValue 
>   (variety,name,depends)
>    = 
>   Value name depends variety

>  (dependencyGraph, vertexToNode, _) =
>   graphFromEdges $
>    map
>     (\(Value
>         valueName
>         valueDepends
>         valueVariety) ->
>        (valueVariety,
>         valueName,
>         valueDepends))
>     values

>  sortedValues ::
>   [Value]
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
>    [Value]

>  checkedDependsEither =
>   sequence $
>    map
>     checkDepends
>     sortedValues

>  checkDepends ::
>   Value ->
>   Either
>     Error
>     Value

>  checkDepends
>   value
>    =
>   let
>    resolvedDepends
>      =
>     takeWhile
>      checkDepend
>      $ valueDepends
>         value
>   in
>   case evaluationType $ valueVariety value of
>    Evaluated ->
>     case length (valueDepends value) == length resolvedDepends of
>      True ->
>       Right
>        value
>      False ->
>       Left
>        ("Error, could not resolve the dependencies of these\n" ++
>        "evaluated values:" ++
>        (concatMap
>         (\depend->
>           ' ':depend)
>         $ valueDepends value \\ resolvedDepends))
>    Static ->
>     Right
>      (value{
>       valueDepends
>         =
>        resolvedDepends})

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
