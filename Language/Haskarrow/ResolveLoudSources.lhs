>{-# LANGUAGE GADTs #-}
>module Language.Haskarrow.ResolveLoudSources
> (resolveLoudSources) where

>import Language.Haskarrow.Types
>import Data.List (find,groupBy,sortBy,mapAccumR)
>import Control.Error.Util (note)
>import Data.Functor
>import Data.Maybe

>resolveLoudSources ::
> [Value a Unresolved] ->
> Either
>  Error
>  [Value a Resolved]

>resolveLoudSources
> values
>   =
> mapM
>  (resolveLoudSourcesForValue values)
>  values

>resolveLoudSourcesForValue ::
> [Value a Unresolved] ->
> (Value a Unresolved) ->
> Either
>  Error
>  (Value a Resolved)

>resolveLoudSourcesForValue
> values
> value@Value{
>  listenerInfluences =
>   (UnresolvedInfluences influences@(_:_)),
>  valueVariety=ValueVariety{
>                timeOfEvaluation = DeReReEval}}
>  =
> let
>  ungroupedSources ::
>   Either
>    Error
>    [Influence]

>  ungroupedSources =
>   concat <$>
>    mapM
>     findSources
>     influences

>  findSources ::
>   String ->
>   Either
>    Error
>    [Influence]

>  findSources
>   influences
>    =
>   (map
>    (\source ->
>       Influence
>        (Source (SourceID 0,source))
>        [influences]))
>       <$>
>    (findSources' influences)

>  findSources' ::
>   String ->
>   Either
>    Error
>    [String]

>  findSources'
>   influence
>     =
>   do
>    found <- note
>     ("")
>     $ find
>        (\value -> valueName value == influence)
>        values

>    case found of
>     Value{
>      listenerInfluences=(UnresolvedInfluences []),
>      valueVariety=ValueVariety
>       {timeOfEvaluation=DeReReEval}} ->
>        Right [influence]

>     Value{
>      listenerInfluences=(UnresolvedInfluences influencesOfInfluence),
>      valueVariety=ValueVariety
>       {timeOfEvaluation=DeReReEval}} ->
>       concat
>         <$>
>        mapM
>         findSources'
>         influencesOfInfluence
>     _ ->
>      Left ("Cannot listen to " ++ influence ++ "it is not a loud object.")

>  groupedSources ::
>   [Influence] ->
>   [Influence]

>  groupedSources
>   ungroupedInfluences
>     =
>   map
>    flattenGroup
>    $ groupBy
>     (\influence1 influence2 ->
>        sourceName influence1
>              ==
>        sourceName influence2)
>     $ sortBy
>       (\influence1 influence2 ->
>          sourceName influence1
>               `compare`
>          sourceName influence2)
>       ungroupedInfluences
>  flattenGroup ::
>   [Influence] ->
>   Influence
>  flattenGroup
>   influences@(influence:_)
>    =
>   Influence{source=lookUpSource (sourceName influence) (sources values),
>    dependentInfluences=
>     concatMap
>      dependentInfluences
>      influences}
> in
> (\influences->value{listenerInfluences=
>  ResolvedInfluences(influences)})
>    <$>
> (groupedSources <$> ungroupedSources)

>resolveLoudSourcesForValue
> _
> value
>   =
> Right
>  value{
>   listenerInfluences=(ResolvedInfluences [])}

>sources
>  ::
> [Value a Unresolved]->
> [Source]
>sources
> values
>  =
> snd $
>  mapAccumR
>   setSourceID
>   0
>   (mapMaybe
>     sourceMaybe
>     values)

>setSourceID
>  ::
> Int ->
> Source ->
> (Int, Source)

>setSourceID
> acc
> (Source (_,name))
>  =
> (acc+1,Source (SourceID acc,name))

>sourceMaybe
>  ::
> Value a Unresolved ->
> Maybe Source

>sourceMaybe
> (Value{
>   valueName = name,
>   listenerInfluences= (UnresolvedInfluences []),
>   valueVariety=
>    ValueVariety{timeOfEvaluation=DeReReEval}})
>  =
> Just (Source (SourceID 0,name))

>sourceMaybe _ = Nothing

> -- | Look up the source by name.  WARNING, uses fromJust!
>lookUpSource
>  ::
> String ->
> -- ^ Source name.
> [Source] ->
> Source
>lookUpSource
> name
> theseSources
>  =
> fromJust
>  $ find
>     (\(Source (_,thisSourceName))->
>        thisSourceName
>          ==
>         name)
>    theseSources
