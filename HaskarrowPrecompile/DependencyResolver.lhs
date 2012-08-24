>module HaskarrowPrecompile.DependencyResolver where
>import HaskarrowPrecompile.Types

>import Data.List
>import Data.Graph

>resolveDependencies ::
> [Value] ->
> Either [Value] Error

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
>    [Value]
>    Error

>  checkedDependsEither =
>   let

This is reversed because I didn't really undestand how to take advantage of liftM2 when I started this project, so I didn't put my Eithers in the right order.

>    reversedEither
>      =
>     sequence $
>      map
>       checkDepends
>       sortedValues
>   in
>   case reversedEither of
>    Left
>     error ->
>      Right
>       error
>    Right
>     value ->
>      Left
>       value

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
