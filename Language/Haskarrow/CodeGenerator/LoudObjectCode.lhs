>module Language.Haskarrow.CodeGenerator.LoudObjectCode where
>import Language.Haskarrow.Types
>import Language.Haskarrow.CodeGenerator.GeneralFunctions
>import Data.List

>loudObjectListenerFunctionsCode ::
> [Value Resolved Resolved] ->
> Indentation               ->
> Code

>loudObjectListenerFunctionsCode
> ((Value{
>   valueName=name,
>   listenerInfluences=(ResolvedInfluences influences),
>   valueVariety=ValueVariety{
>                 timeOfEvaluation=DeReReEval}}):vs)
> indent
>  =
> "\n"++
> (indentation indent)++
> name++
> " draw_'_ " ++
> (unwords
>   $ map
>      allInfluencesCode
>      influences) ++
> " =\n" ++
> (indentation indent) ++
> "do\n"++
> (indentation (increaseIndentation indent))++
> "self_'_ <- newLoudListener\n"++
> (unlines
>   $ snd (mapAccumR
>     (withLoudObjectCode indent)
>     InitialInfluence
>     influences)) ++
> "\n"++
> (indentation
>  ((increaseIndentation . increaseIndentation)
>    indent))++
> "forkIO\n"++
> (indentation (increaseIndentation indent))++
> "return self_'_\n"++
> (indentation indent)++
> "where\n"++
> (indentation
>  (iterate increaseIndentation indent !! 2))++
> "thisListener_'_\n"++
> (indentation
>  (iterate increaseIndentation indent !! 3))++
> "self_'_ "++
> (unwords
>   (map
>     (influenceListenerParameterCode indent)
>     influences))++
> "\n"++
> (indentation
>  (iterate increaseIndentation indent !! 6))++
> "=\n"++
> (indentation
>  (iterate increaseIndentation indent !! 3))++
> "do\n"++
> (indentation
>  (iterate increaseIndentation indent !! 4))++
> "((draw_'_ "++
> (directInfluencesCode influences)++
> ") >>= setLoudListenerValue self_'_\n"++
> (indentation
>  (iterate increaseIndentation indent !! 4))++
> "(beenReadChan_'_,updatedSources_'_)<-atomically(\n"++
> (indentation
>  (iterate increaseIndentation indent !! 5))++
> "(listenForTouch self_'_)"++
> (listenSourcesAndPeekInfluencesCode indent influences)++
> ")\n"++
> (continuationCasesCode indent influences)++
> (loudObjectListenerFunctionsCode vs indent)

>loudObjectListenerFunctionsCode
> (_:los)
> indent
>  =
> "\n"++
> (loudObjectListenerFunctionsCode los indent)

>loudObjectListenerFunctionsCode
> []
> _
>  =
> ""

>allInfluencesCode
>  ::
> Influence ->
> Code
>allInfluencesCode
> (Influence
>   (Source (_,sourceName))
>   listeners)
>  =
> " "++
> sourceName++
> " "++
> (unwords
>   listeners)++
> " "

>data InfluenceGeneratorAcc =
>   InitialInfluence
> | FurtherInfluence

>withLoudObjectCode
>  ::
> Indentation->
> InfluenceGeneratorAcc->
> Influence ->
> (InfluenceGeneratorAcc,Code)
>withLoudObjectCode
> indent
> acc
> (Influence (Source (_,name)) listeners)
>  =
> (FurtherInfluence,(indentation
>  (iterate increaseIndentation indent !! 0))++
> "(withLoudSource\n"++
> (indentation
>  (iterate increaseIndentation indent !! 1))++
> name++
> (case acc of
>   InitialInfluence -> "(thisListener_'_ self_'_)"
>   FurtherInfluence -> "")++")>>=\n"++
> concatMap
>  (\listener ->
>   (indentation
>    (iterate increaseIndentation indent !! 0))++
>    "(withLoudListener "++
>    listener++
>    ")>>=\n")
>  listeners)

>influenceListenerParameterCode
>  ::
> Indentation ->
> Influence ->
> Code
>influenceListenerParameterCode
> indent
> influence
>  =
> influenceListenerParameterCodeWithSuffixes
>  indent
>  influence
>  ""

>influenceListenerParameterCodeWithSuffixes
>  ::
> Indentation ->
> Influence   ->
> String      ->
> Code

>influenceListenerParameterCodeWithSuffixes
> indent
> (Influence (Source ((_,name))) listeners)
> suffix
>  =
> let
>  params = name:listeners
> in
>  concatMap
>   (\param->
>    " ("++
>    param++
>    "_'_,"++
>    param ++
>    "Val"++
>    suffix++
>    "_'_) ")
>   params

>directInfluencesCode
>  ::
> [Influence] ->
> Code
>directInfluencesCode
> influences
>  =
> unwords $ map dIC influences
> where
>  dIC
>   (Influence (Source ((_,source))) [])
>    =
>   source
>  dIC
>   (Influence (Source (_,_)) listeners)
>    =
>   " "++unwords listeners

>listenSourcesAndPeekInfluencesCode
>  ::
> Indentation ->
> [Influence] ->
> Code
>listenSourcesAndPeekInfluencesCode
> indent
> influences
>  =
> unlines $ map (lsapic indent) influences
> where
>  lsapic
>   indent
>   (Influence (Source (SourceID id,source)) [])
>    =
>   (indentation
>    (iterate increaseIndentation indent !! 5))++
>   "`orElse`(((,) (SourceID "++(show id)++"))"++
>   " `fmap` (readTChan $ updatesChan " ++ source++"))\n"
>  lsapic
>   indent
>   (Influence s@(Source (_,_)) listeners)
>    =
>   lsapic indent (Influence s [])++
>   concatMap
>    (\influence ->
>     (indentation
>      (iterate increaseIndentation indent !! 5))++
>     "`orElse`(peekInfluence "++
>     influence++
>     ")\n")
>     listeners

>continuationCasesCode
>  ::
> Indentation ->
> [Influence] ->
> Code
>continuationCasesCode
> indent
> influences
>  =
> (indentation
>  (iterate increaseIndentation indent !! 4))++
> "case (beenReadChan_'_, updatedSources_'_) of\n"++
> (indentation
>  (iterate increaseIndentation indent !! 5))++
> "((SourceID (-1)),_) -> do"++
> (unlines $ map
>  (\listener ->
>    (indentation
>     (iterate increaseIndentation indent !! 6))++
>    listener++
>    "Val'_'_ <- flushTChan (listenerValuesChan "++
>    listener++
>    "_'_)"++
>    listener++
>    "Val_'_\n")
>  (allListeners influences)) ++
> (indentation
>  (iterate increaseIndentation indent !! 6))++
> "thisListener_'_ self_'_"++
> "TODO!"

>allListeners
>  ::
> [Influence] ->
> [String]
>allListeners
> influences
>  =
> nub $ concatMap
>  (\(Influence _ listeners)->listeners)
>  influences
