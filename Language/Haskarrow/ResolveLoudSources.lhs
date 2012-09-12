>{-# LANGUAGE GADTs #-}
>module Language.Haskarrow.ResolveLoudSources
> (resolveLoudSources) where

>import Language.Haskarrow.Types
>import Data.List (find,groupBy,sortBy,mapAccumR)
>import Control.Error.Util (note)
>import Data.Functor
>import Data.Maybe
>import Control.Monad.Instances

>import Debug.Trace

We take a list of values, each of which may be DeReReEval but need not be.

>resolveLoudSources ::
> [Value a Unresolved] ->
> Either
>  Error
>  [Value a Resolved]

>resolveLoudSources
> values
>   =

For each value, we resolve it's sources.

> mapM
>  (resolveLoudSourcesForValue values)
>  values

>resolveLoudSourcesForValue ::
> [Value a Unresolved] ->
> (Value a Unresolved) ->
> Either
>  Error
>  (Value a Resolved)

But only if it has any, aka, it is a DeReReEval value with a
non-empty list of influences.

>resolveLoudSourcesForValue
> values
> value@Value{
>  listenerInfluences =
>   (UnresolvedInfluences influences@(_:_)),
>  valueVariety=ValueVariety{
>                timeOfEvaluation = DeReReEval}}
>  =
> let

First we gather a shallow tree of source listener pairings. Where the listeners are the influences of this value:

Say we have:

source1 =% 1

source2 =% 2

listener1 <!> source2 =% source2+2

listener2 <!> source1 listener1 =% source1 + linstener1


And we are resolving the influences for listener2.

Before we resolve these influences we have a naive list like:

influences = ["source1","listener1"]

We want to actually listen to changes that happen at the source level, rather than at the listener level. This is to prevent duplicate updates.

So we want to change our naive list of influences into:

[Influence (Source (SourceID,"source1")) [],
 Influence (Source (SourceID,"source2")) ["listener1"]]

This is necesary because we want listener2 to only actually listen to source1 and source2 and not listener1.  This way, if two listeners which listener2 is influenced by are updated by the same source, for each actual update we are only updated once!

>  ungroupedSources ::
>   Either
>    Error
>    [Influence]

>  ungroupedSources =
>   concat <$>
>    mapM
>     findSources
>     influences

Finding the actual sources to a given value is a recursive task. First we go down our list of influences.  And we add those influences which are sources to our list.  But then, we also must look at those influences which are themselves listeners, and search their influences for sources, ad infinitum.  REALLY ad infinitum if we have recursive listening!

>  findSources ::
>   String ->
>   Either
>    Error
>    [Influence]

For each influence which is a listener, find it's sources. Noting, that it may itself have multiple sources.

>  findSources
>   influences
>    =
>   concat
>    <$>
>   mapM
>     (\influence ->
>        findSources'
>         influence
>         influence)
>     influences


>  findSources' ::
>   String ->
>   String ->
>   Either
>    Error
>    [Influence]

>  findSources'
>   origionalInfluence
>   -- ^ This is a recursive funciton.  We keep track of the origional influence so that we can check if we are recusing infinitely.
>   influence
>     =
>   do
>    found <- note
>     ("Error: node declared to listen to non-existant value:"++influence)
>     $ find
>        (\value -> valueName value == influence)
>        values

If this influence is itself a source, then we are done processing it.

>    case found of
>     Value{
>      listenerInfluences=(UnresolvedInfluences []),
>      valueVariety=ValueVariety
>       {timeOfEvaluation=DeReReEval}} ->
>        Right [Influence (Source (SourceID 0,influence)) []]

If it is a listener however, we must find IT's sources.

>     Value{
>      listenerInfluences=(UnresolvedInfluences influencesOfInfluence),
>      valueVariety=ValueVariety
>       {timeOfEvaluation=DeReReEval}} ->
>       concat
>         <$>
>        mapM
>         (findSources' origionalInfluence)
>         influencesOfInfluence
>     _ ->
>      Left ("Cannot listen to " ++ influence ++ "it is not a loud object.")

But even the ungroupedSources function is too naive.  It does not group our new pairings  So we will end up with cases like:

[Influence (Source (SourceID,"source1")) [],
 Influence (Source (SourceID,"source2")) ["listener1"],
 Influence (Source (SourceID,"source2")) ["listener3"]]

The grouped sources function deals with this issue.

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

The groupBy above gives us a list like:

[Influence (Source (SourceID,"source2")) ["listener1"],
 Influence (Source (SourceID,"source2")) ["listener3"]]

And flattenGroup gives us a list like:

Influence (Source (SourceID,"source2")) ["listener1","listener3"]]

>  flattenGroup ::
>   [Influence] ->
>   Influence
>  flattenGroup
>   influences@(influence:_)
>    =

>   Influence{source=

We look up the source in our special list of sources which have unique SourceID's attached.  Before we had just filled this number with 0.

>              lookUpSource
>               (sourceName influence)
>               (sources values),
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
