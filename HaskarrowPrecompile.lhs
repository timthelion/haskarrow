>module HaskarrowPrecompile where

import Debug.Trace

require her-lexer >= 0.1 from cabal

>import Language.Haskell.Her.HaLay
>import Language.Haskell.Her.FromClutterToLines

require cpphs from cabal

>import Language.Preprocessor.Unlit

require split from cabal

>import Data.List.Split
>import Data.List

>import Data.Graph

This is in unprocessed form.  That is, it maintains the IO type and any newlines.

>type Type = [Tok]

>data Value = Value {
> valueType    :: Type    ,
> valueName    :: String  ,
> valueDepends :: [String],
> valueVariety :: ValueVariety} deriving (Show)

Haskarrow code preprocesses/precompiles to Haskell code.  In this process, all values are renamed, adding "Value" to the end.

>valueSuffix = "Value"

A data constructor is then created, named "Values" which will hold the post evaluation/post deparameterized values of all the values in the module.

>valuesDataConstructorName = "Values"

A function named "initValues" of type "Parameter -> Maybe Parameter -> .. -> .. -> IO Values" or "Parameter -> Maybe Parameter -> .. -> .. -> Values" depending on if there are any evaluated values in the haskarrow module or not is created.

>initValuesFunctionName = "initValues"

The initValues function evaluates the EvaluatedValues in the haskarrow module in the order determined by dependency resolution like in the example bellow.

initValues :: IO Values
initValues = do
 foo <- fooValue
 bar <- barValue
 foobar <- foobarValue foo bar
 

A main statement is also generated if possible.  See "mainCode" bellow.


WARNING! NOTE! CAUTION! ACHTUNG! POZOR! EEEK A SNAKE! WATCH OUT! WET FLOOR!

For the sake of not conflicting with the literate syntax of this file, I have written the example lines of haskarrow code without the >'s before them.  However, you should note, that haskarrow is normally written in the literate style WITH the >'s.

There are values declared like this:

myEvaluatedValue :: Foo -> Bar -> IO Lollypop
myEvaluatedValue foo bar <<
 do me

These values are resolved when initValues is run.  No questions asked.

Then there are values that are declared like this:

myMaybeParametricEvaluatedValue :: Foo -> IO IceCream
myMaybeParametricEvaluatedValue foo <?<
 do me

Functions that have these types are passed as arguments to initValues.  For example if you wanted to pass your OWN version of myMaybeParametricEvaluatedValue, you would pass a value of type (Maybe (Foo -> IO IceCream)) to initValues.  If you DIDN'T want to use your own version, you would pass Nothing.

Then there are values that are declared like THIS:

myParametricEvaluatedValue :: Lollipop -> IO Lollipop
myParametricEvaluatedValue lollipop <?

You see, there wasn't a "do me" after the <? There isn't one.  These values are non optional arguments of initValues.  If there is even just one parametric value in your entire module, guess what? Haskarrow cannot generate a main statement for your module, because it's missing a value.  This is only used if you are going to be creating non independent application parts....

Now we've seen that there are three different types of EvaluatedValue's.  Normal EvaluatedValues, MaybeParametricEvaluatedValues, and ParametricEvaluatedValues.  Despite the fact that haskarrow's origional aim's of providing dependency resolution based order of evaluation, haskarrow also provides us with a great opportunity to add support for "parametric modules".  Modules in haskarrow, which are entirely outside of the IO monad and for which order of evaluation is meaningless, but which benefit from the ease of making Parametric and Maybe Parametric functions who's values are determined at run time.

So for completenesses sake.  We have three non evaluated equivalents to the syntax for evaluated values described above:

myStaticValue :: Woot -> Bzaptdphlegm -> [Bzaptdphlegm]
myStaticValue woot bzaptdphlegm = woot bzaptdphlegm : []

= Is roughly equivalent to <<

This is very similar to the syntax of a normal haskell top level declaration.  It's not the same however.  I appologize.   The mixture of the dependency resolution based evaluation method and the parametric modules complicates things.  When we are talking about EvaluatedValues, haskarrow always wishes to end up with a value of type (IO a), so that it can evaluate these values.  However, when we are talking about StaticValues or non evaluated values haskarrow is only interested in a type of (a).  It is often usefull to have a top level value of type (a -> b) for example ;) (DUH!) But we're using parametric modules, and haskarrow is already set up to try to resolve everything that comes before an (->) as a dependency!  The solution I came up with, is the idea of "resolve what you can."  This is, in a haskarrow file containing the lines:

label :: IO Label
label << labelNew (Just "foo")

setLabelText :: Label -> String -> IO ()
setLabelText label text = labelSetText label text

Of the dependencies/parameters of setLabelText ["label","text"] only one of them can be resolved "label", no value named "text" in our haskarrow module exists.  In the end, we will be given a value of type (String -> IO ()).

The next problem with this approach is that of cyclical dependency resolution.  StaticValues don't have to worry about this, and can benefit from depending upon eachother.

Our previous example is compiled like this:

initValues :: IO ()
initValues = do
 label <- labelValue
 let setLabelText = setLabelTextValue label

What if we had another Static Value which was mutually dependent on setLabelText.  It would be necessary, that this value be in the same let clause as setLabelText.  I have tried to do this. I have not proven, however, that mutually dependent values are grouped. TODO TODO TODO TODO :/  I really don't know how to prove this.

So as I said,

= is semi-equivalent to <<

now

=?= is similar to <?<

and

=? is like <?

>data ValueVariety =
> ValueVariety{
>  evaluationType :: EvaluationType,
>  isParameter    :: IsParameter}
>  deriving(Show,Eq)

>data EvaluationType = Evaluated | Static
> deriving (Show,Eq)

>data IsParameter =
> InternalValue        |
> MaybeParametricValue |
> CertainlyParametricValue     
>  deriving (Show,Eq)

>data ValueBuilder =
> NoValue                                             |
> ValueWithType Type                                  |
> ValueWithTypeAndNameAndDepends Type String [String] |
> ValueBody deriving (Show)

>type Error = String

Returns either HaskellCode OR AnError

>precompile :: String -> FilePath -> Either String Error
>precompile origionalSource fileName =
> let
>  eitherValuesAndFunctionsOrAnError =
>   expandFunctions origionalSource fileName
> in
>  case eitherValuesAndFunctionsOrAnError of
>   Left (values, functions,topLevel) ->
>    case resolveDependencies values of
>     Left values' ->
>      let
>       evaluated
>         =
>        areAnyValuesToBeEvaluated values'
>      in
>      Left ("{- AUTOGENERATED BY HASKARROW -}"++
>            functions ++ "\n" ++
>            (generateDataDeclaration values' topLevel) ++
>            (generateInit evaluated values' topLevel) ++
>            (if evaluated
>              then
>               (mainCode topLevel values')
>              else "{-NoMain, nothing to do.-}"))
>     Right error -> Right error
>   Right error -> Right error

Returns what it should, or an error message.

| OK, the idea of this function, is that we look through the lines. When we get to a line that meets our pattern(beggining with a Lid), we start actually processing.  We add lines begining with a Lid to our our list of Values, and we transform the source code into proper haskell at the same time.  We then use these values later on to do even more source code generation.

>expandFunctions ::
> String   ->
> FilePath ->
> Either ([Value], String, Int) Error

>expandFunctions
> origionalSource
> fileName = 

> let

We take the Toks generated by her-lexer

>  tokss :: [[Tok]]
>  tokss =
>   fromClutterToLines $
>    ready fileName $
>     unlit fileName origionalSource

>  eatTillLidFoundThenContinueProcessingNormally ::
>   [[Tok]] ->
>   [[Tok]] ->
>   Either ([Value],String,Int) Error

>  eatTillLidFoundThenContinueProcessingNormally
>   processedToks
>   ((toks@((NL _):(Spc _):(Lid _):_)):tokss) =
>   processToks
>    processedToks
>    (toks:tokss)
>    (indentLevel toks)
>    NoValue
>    []

>  eatTillLidFoundThenContinueProcessingNormally
>   processedToks
>   ((toks@((NL _):(Lid _):_)):tokss) =
>   processToks
>    processedToks
>    (toks:tokss)
>    (indentLevel toks)
>    NoValue
>    []

>  eatTillLidFoundThenContinueProcessingNormally
>   processedToks
>   (toks:tokss) =
>   eatTillLidFoundThenContinueProcessingNormally
>    (processedToks++(toks:[]))
>    tokss

>  eatTillLidFoundThenContinueProcessingNormally
>   _
>   [] =
>   Right
>    ("Error, no values found, are you sure this is a valid haskarrow file?\n" ++
>    (show tokss))

>  processToks ::
>   [[Tok]]      ->
>   [[Tok]]      ->
>   Int          ->
>   ValueBuilder ->
>   [Value]      ->
>   Either ([Value],String,Int) Error

If our ValueBuilder currently has no value, than we are expecting to find a type.

>  processToks
>   processedToks
>   (toks:tokss)
>   topLevel
>   NoValue
>   processedValues =

If the line contains a keyword(KW) we ignore it.

>   let
>    keyword =
>     any (\tok ->
>           case tok of
>            KW _ -> True
>            _    -> False) toks

>    eatType ::
>     [Tok] ->
>     Either ([Tok],[Tok]) Error
>    eatType
>     toks =
>      let
>       typeToks' =
>        filter (\tok->
>                 case tok of
>                  (T Ty _) -> True
>                  _        -> False)
>               toks
>       fullToks =
>        map (\tok ->
>             case tok of
>              (Lid name) -> (Lid (name++valueSuffix))
>              tok'       -> tok')
>            toks
>      in
>      if not $ null typeToks'
>       then 
>        let
>         ((T Ty typeToks):_) =
>          typeToks'
>        in
>         Left
>          (fullToks,typeToks)
>       else
>        Right
>         ("Could not parse the line:\n"++
>         (toksOut toks) ++
>         "\nRemember that haskarrow ALWAYS expects a type declaration."++
>         "And that this type declaration should come before the value.")
>   in
>    case (eatType toks,keyword) of
>     (Left (fullToks,typeToks),False) ->
>      let 

This is the most beautiful(I mean hackish) use of laziness ever...  We hand processToksSniff our list of processedToks, even though we CANNOT yet actually populate that list, because we are waiting on the result of processToksSniff to tell us if our current value is a parametric value value which must be ignored.

>       (ignore,normalResult) =
>        processToksSniff
>         (processedToks++
>          ((if ignore
>             then
>              Sym "{-" : fullToks
>             else
>              fullToks):[]))
>         tokss
>         topLevel
>         (ValueWithType typeToks)
>         processedValues 
>      in
>       normalResult

Ignore keyword lines.

>     (_, True) ->
>      processToks
>       (processedToks++(toks:[]))
>       tokss
>       topLevel
>       NoValue
>       processedValues 
>     (Right errorString, _) ->
>      Right errorString 

WARNING!  This is out of order.  If you are reading this source code, jump down to processToksSnif, and then come back up here and keep reading.

Here we eat the body of the value, if any.

>  processToks
>   processedToks
>   (toks:tokss)
>   topLevel
>   ValueBody
>   processedValues =
>   if indentLevel toks /= topLevel

We actually end up just ignoring the body of the value declaration.

>    then
>     processToks
>      (processedToks++(toks:[]))
>      tokss
>      topLevel
>      ValueBody
>      processedValues
>    else
>     processToks
>      processedToks
>      (toks:tokss)
>      topLevel
>      NoValue
>      processedValues

If we run out of lines to process, then we're done :)

>  processToks
>   processedToks
>   []
>   topLevel
>   NoValue
>   processedValues =
>   Left
>    (processedValues,tokssOut processedToks,topLevel)

>  processToks
>   processedToks
>   []
>   topLevel
>   ValueBody
>   processedValues =
>   Left
>    (processedValues,tokssOut processedToks,topLevel)

>  processToks
>   processedToks
>   []
>   topLevel
>   valueBuilder
>   processedValues =
>   Right
>    ("Incomplete Value declaration, end of file reached unexpectedly. The final value builder object was:\n"++
>    (show valueBuilder))

Returns True if the value turns out to be a ? which has to be ignored, or False if the value turns out to be a normal value...

>  processToksSniff ::
>   [[Tok]]      ->
>   [[Tok]]      ->
>   Int          ->
>   ValueBuilder ->
>   [Value]      ->
>   (Bool,Either ([Value],String,Int) Error)

>  processToksSniff
>   processedToks
>   (toks:tokss)
>   topLevel
>   (ValueWithType typeToDate)
>   processedValues =

If we are currently processing a type, then we check if our current line is of a level bellow the top.  If so, the code is probably invalid but we ignore this since I cannot think of a proof that the code is invalid so I'll just let GHC choke on it, otherwise we start processing the value.


>   if indentLevel toks > topLevel
>    then
>     processToksSniff
>      (processedToks++(toks:[]))
>      tokss
>      topLevel
>      (ValueWithType typeToDate)
>      processedValues
>    else

When processing that value, we can either complete the opperation in this round, if we find a symbol(aka <<,<?<,<?,=,=?=,=,?), or we have to continue building our list of depends untill we come to a symbol.

>     case scoopDeclarationAndDependsWithRename toks of
>      Left ((name:depends),toks',Nothing) -> 

Symbol not yet found, continue looking...

>       processToksSniff
>        (processedToks++(toks':[]))
>        tokss
>        topLevel
>        (ValueWithTypeAndNameAndDepends
>          typeToDate
>          name
>          depends)
>        processedValues

>      Left ((name:depends),toks',
>           Just variety@
>                 (ValueVariety
>                  _
>                  CertainlyParametricValue)) ->
>       (True,
>        processToks
>         (processedToks++(toks':[]))
>         tokss
>         topLevel
>         ValueBody
>         ((Value
>           typeToDate
>           name
>           depends
>           variety):processedValues))

>      Left ((name:depends),toks',Just variety) ->
>       (False,
>        processToks
>         (processedToks++(toks':[]))
>         tokss
>         topLevel
>         ValueBody
>         ((Value
>           typeToDate
>           name
>           depends
>           variety):processedValues))

>      Left (_, [],_)   ->
>       (False,
>        Right
>         ("Error parsing line:\n"++
>         (toksOut toks)++
>         "\nExpected a value declaration, no viable value name found."))
>      Right scoopError ->
>       (False,Right scoopError)

This only happens if we have multiple lines before the <<

>  processToksSniff
>   processedToks
>   (toks:tokss)
>   topLevel
>   (ValueWithTypeAndNameAndDepends
>     typeToDate
>     name
>     depends)
>   processedValues
>    =
>   case scoopDeclarationAndDepends toks of

>    Left (depends',toks', Just variety@
>                                (ValueVariety
>                                 _
>                                 CertainlyParametricValue)) ->
>     (True,
>      processToks
>       (processedToks++(toks':[]))
>       tokss
>       topLevel
>       ValueBody
>       ((Value
>          typeToDate
>          name
>          (depends++depends')
>          variety):processedValues))

>    Left (depends',toks', Just variety) ->
>     (False,
>      processToks
>       (processedToks++(toks':[]))
>        tokss
>        topLevel
>        ValueBody
>        ((Value
>           typeToDate
>           name
>           (depends++depends')
>           variety):processedValues))

>    Left (depends',toks', Nothing) ->
>     processToksSniff
>      (processedToks++(toks':[]))
>      tokss
>      topLevel
>      (ValueWithTypeAndNameAndDepends
>        typeToDate
>        name
>        (depends++depends'))
>      processedValues

>    Right scoopError ->
>     (False,Right scoopError)

>   
> in
>  eatTillLidFoundThenContinueProcessingNormally
>   []
>   tokss

| From a line like:

foo someDepend anotherDepend << bar

scoop the name and the depends returning:

Left (["foo", "someDepend","anotherDepend"],[processedToks], (Just IOValue))

From a line like:

foo someDepend
    anotherDepend << Bar

Return:

Left (["name","someDepend"],[processedToks]), Nothing)

>scoopDeclarationAndDepends ::
> [Tok] ->
> Either ([String],[Tok],Maybe ValueVariety) Error

>scoopDeclarationAndDepends
> toks =
> scoopDeclarationAndDepends'
>  toks
>  id
>  []
>  []

Because we would get conflicting names if our functions where named the same as the values(record selectors) that they produced, we have to rename the functions.

>scoopDeclarationAndDependsWithRename ::
> [Tok] ->
> Either ([String],[Tok],Maybe ValueVariety) Error

>scoopDeclarationAndDependsWithRename
> toks
>   =
> scoopDeclarationAndDepends'
>  toks
>  (\name->name++valueSuffix)
>  []
>  []

>scoopDeclarationAndDepends' ::
> [Tok]               ->
> -- ^ Tokens to be processed.
> (String -> String)  ->
> -- ^ Function to rename any top level declaration.
> [String]            ->
> -- ^ Dependency accumulator.
> [Tok]               ->
> -- ^ Already processed Tokens.
> Either ([String],[Tok], Maybe ValueVariety) Error
> -- ^ (Dependencies, processed toks, ValueVariety if equivalency symbol reached.) or an Error

>scoopDeclarationAndDepends'
> ((Lid name):toks)
> rename
> []
> processedToks
>  =
> scoopDeclarationAndDepends'
>  toks
>  rename
>  (name:[])
>  (processedToks++
>   ((Lid (rename name)):[]))

>scoopDeclarationAndDepends'
> ((Lid nameOrDepend):toks)
> rename
> nameAndDepends
> processedToks
>  =
> scoopDeclarationAndDepends'
>  toks
>  rename
>  (nameAndDepends ++
>   (nameOrDepend:[]))
>  (processedToks ++
>   ((Lid nameOrDepend):[]))

>scoopDeclarationAndDepends'
> ((Sym sym):toks)
> _
> nameAndDepends
> processedToks
>  =
> let
>  valueVarietyMaybe
>     =
>   case sym of
>    "<<" ->
>     Just (ValueVariety
>       Evaluated
>       InternalValue)
>    "<?<" ->
>     Just (ValueVariety
>       Evaluated
>       MaybeParametricValue)
>    "<?" ->
>     Just (ValueVariety
>       Evaluated
>       CertainlyParametricValue)
>    "=" ->
>     Just (ValueVariety
>       Static
>       InternalValue)
>    "=?=" ->
>     Just (ValueVariety
>       Static
>       MaybeParametricValue)
>    "=?" ->
>     Just (ValueVariety
>       Static
>       CertainlyParametricValue)
>    _ -> Nothing

> in 
>  case valueVarietyMaybe of
>   Just valueVariety@
>         (ValueVariety
>           _
>           isParameter) ->
>    let
>     newSymbol =
>      case isParameter of
>       CertainlyParametricValue ->
>        "= -}"
>       _ ->
>        "= "
>    in
>     Left
>      (nameAndDepends,
>      (processedToks ++ ((Sym newSymbol):toks)),
>       Just valueVariety)
>   Nothing ->
>    Right
>     ("Error , unexpected symbol in value declaration:\n"++
>      (toksOut ((Sym sym):toks))++
>      "\nThe symble was:\n"++ sym)

scoopDeclarationAndDepends'
 (spc@(Spc _):toks)
 rename
 nameAndDepends
 processedToks
  =
 scoopDeclarationAndDepends'
  toks
  rename
  nameAndDepends
  (processedToks++(spc:[]))

>scoopDeclarationAndDepends'
> (tok:toks)
> rename
> nameAndDepends
> processedToks
>  =
> scoopDeclarationAndDepends'
>  toks
>  rename
>  nameAndDepends
>  (processedToks++(tok:[]))

>scoopDeclarationAndDepends'
> []
> _
> nameAndDepends
> processedToks =
>  Left
>   (nameAndDepends ,
>    processedToks  ,
>    Nothing)

>indentLevel ::
> [Tok] ->
> Int 

>indentLevel
> (nl@(NL _):spc@(Spc indent):toks)
>  =
> length indent

>indentLevel (nl@(NL _):toks) = 0
>indentLevel [] = 0

>areAnyValuesToBeEvaluated ::
> [Value] ->
> Bool
>areAnyValuesToBeEvaluated
> values
>  =
> True

>generateInit ::
> Bool    ->
> -- ^ Evaluated? Is this to be an IO typed function?
> [Value] ->
> -- ^ The values to be inintialized.
> Int     ->
> -- ^ The level of indentation in number of spaces.  No tabs please :P
> String
> -- ^ The source code we generated.

>generateInit
> evaluated
> values
> topLevel
>  =
> "-- | This action loads a haskarrow module.  It returns a giant data constructor with reccords for each value loaded.\n" ++ 
> "-- " ++ initValuesFunctionName ++ " " ++
>  (concatMap
>   (\value->
>    ' ':(valueName value))
>   values) ++ "\n" ++

↑ Everything up there is a comment ↑

 Ha!  I don't need to find the type. It'd be work now that I'm taking arguments.  But GHC will do it for me :D :D :D
 (replicate topLevel ' ') ++ "initValues :: IO Values\n"++

> (replicate topLevel ' ') ++
> initValuesFunctionName ++ " " ++
> (valuesWhichAreTakenAsArguments values) ++ "=" ++

↑ Insert the parameters ↑

> (if evaluated
>  then " do\n"
>  else "\n") ++

> (concatMap
>  (\group ->
>    valueGroupCode group topLevel) $
>  groupValuesByEvaluationType values) ++

↑ Actually create the body of our function. ↑

> (replicate (topLevel+1) ' ') ++
> (if evaluated
>   then "return $ "
>   else "") ++
> valuesDataConstructorName ++
> (concatMap
>  (\value ->
>    ' ':(valueName value))
>  values) ++ "\n"

↑ And return all of our values in a giant data constructor ↑

> where
>  valuesWhichAreTakenAsArguments ::
>   [Value] ->
>   String

>  valuesWhichAreTakenAsArguments values =
>   unwords $
>    map
>     (\name ->
>       name ++ "Parameter") $
>     map valueName $
>      filter
>       (\value ->
>         let
>          amIParameter
>            =
>           isParameter
>            (valueVariety value)
>         in
>          amIParameter == MaybeParametricValue ||
>          amIParameter == CertainlyParametricValue)
>       values

>data EvaluationGroup =
> EvaluatedGroup [Value] |
> StaticGroup    [Value] 

>groupValuesByEvaluationType ::
> [Value] ->
> [EvaluationGroup]

>groupValuesByEvaluationType
> (value:values)
>  =
> case valueVariety value of
>  (ValueVariety
>    Evaluated
>    _) -> 
>   gatherEvaluatedGroup
>    values
>    [value]
>  (ValueVariety
>    Static
>    _) ->
>   gatherStaticGroup
>    values
>    [value]

>groupValuesByEvaluationType
> []
>  =
> []

>gatherEvaluatedGroup ::
> [Value] ->
> -- ^ Unsorted values.
> [Value] ->
> -- ^ Evaluated Values
> [EvaluationGroup]

>gatherEvaluatedGroup
> (value:values)
> inGroupAlready
>  =
> case valueVariety value of
>  (ValueVariety
>   Evaluated
>   _) ->
>    gatherEvaluatedGroup
>     values
>     (inGroupAlready ++ [value])
>  (ValueVariety
>   Static
>   _) ->
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
> [Value] ->
> -- ^ Unsorted values.
> [Value] ->
> -- ^ Static Values
> [EvaluationGroup]

>gatherStaticGroup
> (value:values)
> inGroupAlready
>  =
> case valueVariety value of
>  (ValueVariety
>   Static
>   _) ->
>    gatherStaticGroup
>     values
>     (inGroupAlready ++ [value])
>  (ValueVariety
>   Evaluated
>   _) ->
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
> (take
>   (indent+1) $
>   repeat
>    ' ') ++
> "let\n" ++
> (concatMap
>  (\value ->
>    initValueCode
>     value
>     (indent+1))
>  values) ++
> (take
>   indent $
>   repeat
>    ' ') ++ "\n"

>initValueCode ::
> Value ->
> Int   ->
> String

>initValueCode
> (Value
>   valueType
>   name
>   depends
>   (ValueVariety
>     evaluationType
>     InternalValue))
> topLevel
>  =
> let
>  operator =
>   case evaluationType of
>    Evaluated -> " <- "
>    Static    -> " = "
> in
> (replicate (topLevel+1)' ') ++
> name ++ operator ++
> (name++valueSuffix) ++
> (concatMap
>  (\depend ->
>    ' ':depend ++ " ")
>  depends) ++ "\n"

>initValueCode
> (Value
>  valueType
>  name
>  depends
>  (ValueVariety
>    evaluationType
>    MaybeParametricValue))
> topLevel
>  =
> let
>  operator =
>   case evaluationType of
>    Evaluated -> " <- "
>    Static    -> " = "
> in
> (replicate (topLevel+1)' ') ++ name ++ operator ++ "case " ++
>  name ++ "Parameter of Nothing -> " ++
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
> (Value
>   valueType
>   name
>   depends
>   (ValueVariety
>    evaluationType
>    CertainlyParametricValue))
> topLevel
>  =
> let
>  operator =
>   case evaluationType of
>    Evaluated -> " <- "
>    Static    -> " = "
> in
> (replicate (topLevel+1)' ') ++ name ++ operator ++
> (name ++ "Parameter") ++
> (concatMap
>  (\depend ->
>    ' ':depend ++ " ")
>  depends) ++ "\n"

>generateDataDeclaration ::
> [Value] ->
> Int ->
> String

>generateDataDeclaration
> values
> topLevel
>  =
> (replicate topLevel ' ') ++
> "data " ++
> valuesDataConstructorName ++
> " = " ++
> valuesDataConstructorName ++" {\n" ++

> (tail
>  (concatMap
>   (\value@(Value
>       valueType
>       valueName
>       _
>       _) ->
>    ',' : valueName ++
>          "::" ++
>          (clipType
>            valueType
>            value) ++ "\n")
>   values)) ++ " }\n"

>clipType ::
> [Tok] ->
> Value ->
> String

>clipType
> toks
> (Value
>  _
>  _
>  depends
>  (ValueVariety
>   evaluated
>   _))
>  =

First we split our type string so that we can focus on the last bit which is the only bit we are intrested in.

> let
>  splitByArrows =
>   split
>    (whenElt 
>     (\tok->
>       case tok of 
>        (Sym "->") -> True 
>        _ -> False))
>   toks 
> in
> case evaluated of

For evaluated types, we must remove the parameters and the IO marker, since what we are really storring here is the result of having evaluated that function.

>  Evaluated ->
>    let 
>     (lastType:_)
>       =
>      reverse
>       splitByArrows
>    in

Then we remove that IO and convert the toks into a string of code(getting rid of any new lines).

>    toksOut $ 
>     filter
>      (\elem ->
>        case elem of
>         (NL _) -> False
>         _      -> True) $

>     (fst $
>       foldl
>        (\(foldedToks,found) elem ->
>           if not found && elem == (Uid "IO")
>            then (foldedToks,True)
>            else (foldedToks++(elem:[]),found))
>        ([],False)
>        lastType)

For Static values, we remove as many parameters from the type, as where resolved by our dependency resolver(that is, all of the dependencies still listed after having resolved dependencies)

>  Static ->
>   let
>    clippedType =
>     drop
>      (2 * length depends)
>      splitByArrows

    clippedType =
     case clippedTypeHead of
      (Sym "->") ->
       clippedTypeTail
      _ ->
       (wholeHead:clippedTypeTail)

>   in
>    toksOut $ 
>     filter
>      (\elem ->
>        case elem of
>         (NL _) -> False
>         _      -> True) $
>      concat
>       clippedType

>mainCode ::
> Int ->
> [Value] ->
> String

>mainCode
> topLevel
> values
>  = 
> let
>  numberOfOptionalParameters =
>   length $
>    filter
>     (\value ->
>       isParameter (valueVariety value) == MaybeParametricValue)
>     values

↓ A main statement is only generated if there are Evaluated values to be run, and there are no non-optional parameteres to initValues... Here we only need to check if there are non optional parameters, we checked for the need to evaluate above. ↓

>  canMakeMain =
>   null $
>    filter
>     (\value ->
>       isParameter (valueVariety value) == CertainlyParametricValue)
>     values
> in
> if canMakeMain
>  then
>   (replicate topLevel ' ') ++
>   "main :: IO ()\n"++
>   (replicate topLevel ' ') ++
>   "main = do myValues <- initValues " ++
>   (unwords $
>    take
>     numberOfOptionalParameters $
>     repeat "Nothing") ++
>   " ; (exit myValues) ; return ()\n"
>  else "{-NoMain-}"

>resolveDependencies ::
> [Value] ->
> Either [Value] Error

>resolveDependencies
> values
>  =
> let

Don't be scared, we're just converting the Value's to tuples and back again, so we can use the graph lib to sort this "topoligically".

>  graphNodeTupleToValue 
>   ((n,v),k,ks)
>    = 
>   Value n k ks v

>  (dependencyGraph, vertexToNode, _) =
>   graphFromEdges $
>    map
>     (\(Value
>         valueType
>         valueName
>         valueDepends
>         valueVariety) ->
>        ((valueType,valueVariety),
>          valueName,
>          valueDepends))
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
