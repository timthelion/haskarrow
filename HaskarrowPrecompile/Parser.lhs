>module HaskarrowPrecompile.Parser where

>import Language.Haskell.Her.HaLay
>import Language.Haskell.Her.FromClutterToLines
>import Language.Preprocessor.Unlit
>import Data.List.Split
>import HaskarrowPrecompile.Types
>import HaskarrowPrecompile.CodeGeneratorConstants

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

>equivalencySymbols = ["<<","<?<","<?","=","=?=","=?"]

myStaticValue :: Woot -> Bzaptdphlegm -> [Bzaptdphlegm]
myStaticValue woot bzaptdphlegm = woot bzaptdphlegm : []

= Is roughly equivalent to <<

This is very similar to the syntax of a normal haskell top level declaration.  It's not the same however.  I appologize.   The mixture of the dependency resolution based evaluation method and the parametric modules complicates things.  When we are talking about EvaluatedValues, haskarrow always wishes to end up with a value of type (IO a), so that it can evaluate these values.  However, when we are talking about StaticValues or non evaluated values haskarrow is only interested in a type of (a).  It is often usefull to have a top level value of type (a -> b) for example ;) (DUH!) But we're using parametric modules, and haskarrow is already set up to try to resolve everything that comes before an (->) as a dependency!  The solution I came up with is that there will be an ! exclamation mark between "dependencies" and "normal arguments.  This is, in a haskarrow file containing the lines:

label :: IO Label
label << labelNew (Just "foo")

setLabelText :: Label -> String -> IO ()
setLabelText label ! text = labelSetText label text

Of the dependencies/parameters of setLabelText ["label","text"] only the arguments comming before the ! will be resolved "label".  In the end, we will be given a value of type (String -> IO ()).

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

>data ValueBuilder =
> NoValue                                 |
> ValueWithNameAndPartialDepends ValueName [Dependency] 
> deriving (Show)

>data ProcessedTopLevelDeclarations =
> ProcessedTopLevelDeclarations{
>  processedTopLevelValues :: [Value],
>  processedCode           :: String,
>  processedTopLevelIndent :: Int}

| The idea of this function, is that we look through the lines. When we get to a line that meets our pattern(beggining with a Lid), we start actually processing.  We add lines begining with a Lid to our our list of Values, and we transform the source code into proper haskell at the same time.  We then use these values later on to do even more source code generation.

>expandFunctions ::
> String   ->
> FilePath ->
> Either ProcessedTopLevelDeclarations Error

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

> in
>  eatTillLidFoundThenContinueProcessingNormally
>   []
>   tokss

>eatTillLidFoundThenContinueProcessingNormally ::
> [[Tok]] ->
> [[Tok]] ->
> Either ProcessedTopLevelDeclarations Error

>eatTillLidFoundThenContinueProcessingNormally
> processedToks
> ((toks@((NL _):(Spc _):(Lid _):_)):tokss)
>  =
> processed
> where
>  processToks
>   processedToks
>   (toks:tokss)
>   (indentLevel toks)
>   NoValue
>   []

>eatTillLidFoundThenContinueProcessingNormally
> processedToks
> ((toks@((NL _):(Lid _):_)):tokss)
>  =
> processed
> where
>  (_,processed)=processToks
>   processedToks
>   (toks:tokss)
>   (indentLevel toks)
>   NoValue
>   []

>eatTillLidFoundThenContinueProcessingNormally
> processedToks
> (toks:tokss)
>  =
> eatTillLidFoundThenContinueProcessingNormally
>  (processedToks++(toks:[]))
>  tokss

>eatTillLidFoundThenContinueProcessingNormally
> _
> [] =
> Right
>  ("Error, no values found, are you sure this is a valid haskarrow file?\n" ++
>  (show tokss))

>data CommentPreviousBlock =
> CommentPreviousBlock |
> Don'tCommentPreviousBlock

>processToks ::
> [[Tok]]      ->
> [[Tok]]      ->
> Int          ->
> ValueBuilder ->
> [Value]      ->
> Either
>  (CommentPreviousBlock,
>   ProcessedTopLevelDeclarations)
>  Error

When our ValueBuilder has NoValue, we might find:

1. Some keywords like data, type, newtype(which we ignore).
2. Some non top level stuff, aka, the body of a previous declaration.  We ignore this.
3. A top level Lid.  This could be
 a. A top level declaration's type(which we must rename, or comment out).
 b. A top level declaration which we should process(which we must rename, or comment out).

>processToks
> processedToks
> (toks:tokss)
> topLevel
> valueBuilder
> processedValues =

If the line contains a keyword(KW) which comes before a haskarrow equivalency symbol.  We ignore it.

> let
>  atTopLevel :: Bool
>  atTopLevel = indentLevel toks > topLevel

>  keyword :: Bool
>  keyword =
>   any (\tok ->
>         case tok of
>          KW _     -> True
>          _        -> False)
>       toksBeforeEquivalencySymbol

>  toksBeforeEquivalencySymbol :: [Tok]
>  toksBeforeEquivalencySymbol =
>   head $
>    split
>     (whenElt
>      (\tok->
>        elem
>         tok
>         $ map
>            (\str->
>               (Sym str))
>            equivalencySymbols))
>     toks

>  ignore :: Bool
>  ignore = keyword || not atTopLevel

> in
>  case ignore of
>   False ->
>    let 

If we did find a top level Lid, then we go into another stage where we figure out what it is and what to do with it.

Here are possibilities are:

1. It's a syntax error of some sort.
2. It's a top level value declaration's type.
    - In this case we have to hang arround to find out if the value is a "CertainlyParametricValue".  If so, we need to comment out the type declaration(we already renamed it).
3. It's a top level value declaration.
    - In this case, we also need to hang arround to comment it out in the case of a CertainlyParametricValue.

This is the most beautiful(I mean hackish) use of laziness ever...  We hand processToksSniff our list of processedToks, even though we CANNOT yet actually populate that list, because we are waiting on the result of processToksSniff to tell us if our current value is a parametric value value which must be commented out.

>     (commentOut,normalResult) =
>      case valueBuilder of
>       NoValue ->
>        processToks
>         (processedToks++
>          ((if commentOut == CommentPreviousBlock
>             then
>              Sym "{-" : renamedToks
>             else
>              renamedToks):[]))
>         tokss
>         topLevel
>         NoValue
>         processedValues 
>       --------------------------------------------
>    in
>     (Don'TCommentPreviousLine,normalResult)

If this wasn't a top level Lid, we ignore it.

>   True ->
>    processToks
>     (processedToks++(toks:[]))
>     tokss
>     topLevel
>     NoValue
>     processedValues 

If we run out of lines to process, then we're done :)

>processToks
> processedToks
> []
> topLevel
> NoValue
> processedValues =
> Left
>  (processedValues,tokssOut processedToks,topLevel)

>processToks
> processedToks
> []
> topLevel
> valueBuilder
> processedValues =
> Right
>  ("Incomplete Value declaration, end of file reached unexpectedly. The final value builder object was:\n"++
>  (show valueBuilder))

>processToksSniff ::
> [[Tok]]      ->
> [[Tok]]      ->
> Int          ->
> ValueBuilder ->
> [Value]      ->
> (Bool,Either ([Value],String,Int) Error)

(CommentOutPreviousLine,Either ([ProcessedValues],ProcessedSourceCode,TopIndentationLevel) Error)

>processToksSniff
> processedToks
> (toks:tokss)
> topLevel
> valueBuilder
> processedValues =

If we are currently processing a type, then we check if our current line is of a level bellow the top.  If so, the code is probably invalid but we ignore this since I cannot think of a proof that the code is invalid so I'll just let GHC choke on it, otherwise we start processing the value.

> if indentLevel toks > topLevel
>  then
>   processToksSniff
>    (processedToks++(toks:[]))
>    tokss
>    topLevel
>    valueBuilder
>    processedValues
>  else

When processing that value, we can either complete the opperation in this round, if we find an equivalency symbol, or we have to continue building our list of depends untill we come to a symbol.

>   let
>    keyword :: Bool
>    keyword =
>     any (\tok ->
>           case tok of
>            KW _     -> True
>            _        -> False)
>         toksBeforeEquivalencySymbol

>    toksBeforeEquivalencySymbol :: [Tok]
>    toksBeforeEquivalencySymbol =
>     head $
>      split
>       (whenElt
>        (\tok->
>          elem
>           tok
>           $ map
>              (\str->
>                 (Sym str))
>              equivalencySymbols))
>       toks

>    scoop =
>     case valueBuilder of
>      NoValue ->
>       scoopDeclarationAndDependsWithRename
>      ValueWithNameAndPartialDepends
>        _
>        _ ->
>       scoopDeclarationAndDepends
>         
>    addNameOrDependsToValueBuilder ::
>     [String] ->
>     ValueBuilder
>    addNameOrDependsToValueBuilder
>     (name':depends')
>      =
>     case valueBuilder of
>      ValueWithNameAndPartialDepends
>        name
>        depends ->
>       ValueWithNameAndPartialDepends
>        name
>        (depends++(name':depends'))
>      NoValue ->
>       ValueWithNameAndPartialDepends
>        name'
>        depends'
>   in
>   case (keyword,scoop toks) of
>    (False,PartiallyProcessedDeclaration
>     nameOrDepends
>     toks') -> 

Symbol not yet found, continue looking...

>     processToksSniff
>      (processedToks++(toks':[]))
>      tokss
>      topLevel
>      (addNameOrDependsToValueBuilder
>       nameOrDepends)
>      processedValues

>    (False,ProcessedDeclaration
>     (name:depends)
>     toks' 
>     (variety@
>      (ValueVariety
>       _
>       parametric))) ->
>     (parametric==CertainlyParametricValue,
>      processToks
>       (processedToks++(toks':[]))
>       tokss
>       topLevel
>       NoValue
>       ((Value
>         name
>         depends
>         variety):processedValues))

>    (False,ProcessedDeclaration
>     []
>     toks' 
>     (variety@
>      (ValueVariety
>       _
>       parametric))) ->
>       (False, Right ("Error, declaration processed but no name or dependencies found."++(toksOut toks')))


If we've come across a value's type declaration, we should go back to looking for a top level value declaration.

>    (False,ProcessedValueTypeDeclaration) ->
>     processToksSniff
>      (processedToks++(toks:[]))
>      tokss
>      topLevel
>      NoValue
>      processedValues

Also ignore lines with KW(keywords) in them.

>    (True,_) ->
>     processToksSniff
>      (processedToks++(toks:[]))
>      tokss
>      topLevel
>      NoValue
>      processedValues

>    (False,ProcessingError scoopError) ->
>     (False,Right scoopError)
   
>data ProcessedDeclarationLine =
>   ProcessingError Error
> | ProcessedDeclaration [Dependency] [Tok] ValueVariety
> | PartiallyProcessedDeclaration [Dependency] [Tok]
> | ProcessedValueTypeDeclaration

| From a line like:

foo someDepend anotherDepend << bar

scoop the name and the depends returning:

ProcessedDeclaration ["foo", "someDepend","anotherDepend"] [processedToks] (ValueVariety Evaluated InternalValue)

From a line like the first line in:

foo someDepend
    anotherDepend << Bar

Return:

PartiallyProcessedDeclaration ["name","someDepend"] [processedToks]

From a line like:

foo :: Int

Return:

TypeDeclaration

From a line like the seccond line in:

foo
 :: Int

Return:

TypeDeclaration

Because we would get conflicting names if our functions where named the same as the values(record selectors) that they produced, we have to rename the functions.

>scoopDeclarationAndDependsWithRename ::
> [Tok] ->
> ProcessedDeclarationLine

>scoopDeclarationAndDependsWithRename
> toks
>   =
> scoopDeclarationAndDepends'
>  toks
>  (\name->name++valueSuffix)
>  []
>  []
>  True

>scoopDeclarationAndDepends ::
> [Tok] ->
> ProcessedDeclarationLine

>scoopDeclarationAndDepends
> toks =
> scoopDeclarationAndDepends'
>  toks
>  id
>  []
>  []
>  True

>data DeclarationFamilly =
>   SymbolErrorInDeclaration
> | ValueDeclaration ValueVariety
> | ValueTypeDeclaration

>scoopDeclarationAndDepends' ::
> [Tok]               ->
> -- ^ Tokens to be processed.
> (String -> String)  ->
> -- ^ Function to rename any top level declaration.
> [String]            ->
> -- ^ Dependency accumulator.
> [Tok]               ->
> -- ^ Already processed Tokens.
> Bool                ->
> -- ^ Scoop dependencies?  Remember that after ! we no longer do this.
> ProcessedDeclarationLine

>scoopDeclarationAndDepends'
> ((Lid name):toks)
> rename
> []
> processedToks
> scoop
>  =
> scoopDeclarationAndDepends'
>  toks
>  rename
>  (case scoop of
>    True  -> name:[]
>    False -> [])
>  (processedToks++
>   ((Lid (rename name)):[]))
>  scoop

>scoopDeclarationAndDepends'
> ((Lid nameOrDepend):toks)
> rename
> nameAndDepends
> processedToks
> scoop
>  =
> scoopDeclarationAndDepends'
>  toks
>  rename
>  (nameAndDepends ++
>   (case scoop of
>     True  -> nameOrDepend:[]
>     False -> []))
>  (processedToks ++
>   ((Lid nameOrDepend):[]))
>  scoop

>scoopDeclarationAndDepends'
> ((Sym sym):toks)
> rename
> nameAndDepends
> processedToks
> scoop
>  =
> let
>  declarationFamillyOrProcessedLine ::
>   Either
>    DeclarationFamilly 
>    ProcessedDeclarationLine 
>  declarationFamillyOrProcessedLine
>     =
>   case sym of
>    "!"  ->
>     Right (scoopDeclarationAndDepends'
>      toks
>      rename
>      nameAndDepends
>      processedToks
>      False)
>    "<<" ->
>     Left (ValueDeclaration (ValueVariety
>       Evaluated
>       InternalValue))
>    "<?<" ->
>     Left(ValueDeclaration (ValueVariety
>       Evaluated
>       MaybeParametricValue))
>    "<?" ->
>     Left(ValueDeclaration (ValueVariety
>       Evaluated
>       CertainlyParametricValue))
>    "=" ->
>     Left(ValueDeclaration (ValueVariety
>       Static
>       InternalValue))
>    "=?=" ->
>     Left(ValueDeclaration (ValueVariety
>       Static
>       MaybeParametricValue))
>    "=?" ->
>     Left(ValueDeclaration (ValueVariety
>       Static
>       CertainlyParametricValue))
>    "::" -> Left ValueTypeDeclaration
>    sym  ->
>     case scoop of
>      True  -> Left SymbolErrorInDeclaration
>      False -> 
>       Right (scoopDeclarationAndDepends'
>        toks
>        rename
>        nameAndDepends
>        (processedToks++
>         ((Sym sym):[]))
>        False)

> in 
>  case declarationFamillyOrProcessedLine of
>   Left(ValueDeclaration valueVariety@
>         (ValueVariety
>           _
>           isParameter)) ->
>    let
>     newSymbol =
>      case isParameter of
>       CertainlyParametricValue ->
>        "= -}"
>       _ ->
>        "= "
>    in
>     ProcessedDeclaration
>      nameAndDepends
>      (processedToks ++ ((Sym newSymbol):toks))
>      valueVariety
>   Left ValueTypeDeclaration -> ProcessedValueTypeDeclaration
>   Left SymbolErrorInDeclaration ->
>    ProcessingError
>     ("Error , unexpected symbol in value declaration:\n"++
>      (toksOut ((Sym sym):toks))++
>      "\nThe symble was:\n"++ sym)
>   Right processedLine -> processedLine 

>scoopDeclarationAndDepends'
> (tok:toks)
> rename
> nameAndDepends
> processedToks
> scoop
>  =
> scoopDeclarationAndDepends'
>  toks
>  rename
>  nameAndDepends
>  (processedToks++(tok:[]))
>  scoop

>scoopDeclarationAndDepends'
> []
> _
> nameAndDepends
> processedToks
> _ =
>  PartiallyProcessedDeclaration
>   nameAndDepends
>   processedToks

>indentLevel ::
> [Tok] ->
> Int 

>indentLevel
> (nl@(NL _):spc@(Spc indent):toks)
>  =
> length indent

>indentLevel (nl@(NL _):toks) = 0
>indentLevel [] = 0
