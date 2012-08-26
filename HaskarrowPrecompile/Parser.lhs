>module HaskarrowPrecompile.Parser where

>import Debug.Trace

>import HaskarrowPrecompile.Types
>import HaskarrowPrecompile.CodeGeneratorConstants

>import Language.Haskell.Her.HaLay
>import Language.Haskell.Her.FromClutterToLines

>import Language.Preprocessor.Unlit

>import Data.List.Split
>import Data.List

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

| The idea of this function, is that we look through the lines. When we get to a line that meets our pattern(beggining with a Lid), we start actually processing.  We add lines begining with a Lid to our our list of Values, and we transform the source code into proper haskell at the same time.  We then use these values later on to do even more source code generation.

>type CodeBlock = [CodeLine]
>type CodeLine  = [Tok]
>type Indent    = Int
>type Name = String
>type Code = String

>data ProcessedTopLevelBlock =
>   ProcessedValueDeclaration Value Code
> | ProcessedValueTypeDeclaration Name Code
> | OtherTypeOfProcessedBlock Code

>data Value'sBlockGroup =
>   Value'sBlockGroup{
>    blockGroup'sValue :: Value,
>    blockGropu'sCode  :: Code}
> | OtherBlockGroup
>    Code

>blockGroupsValues ::
> [Value'sBlockGroup] ->
> [Value]

>blockGroupsValues
> blockGroups
>  =
> map
>  blockGroup'sValue
>  $ filter
>     isValue'sBlock
>     blockGroups

>isValue'sBlock ::
> Value'sBlockGroup ->
> Bool

>isValue'sBlock
> Value'sBlockGroup{} = True
>isValue'sBlock
> _ = False

>blockGroupsCode ::
> [Value'sBlockGroup] ->
> Code
>blockGroupsCode
> blockGroups
>  =
> concatMap
>  blockGroupCode
>  blockGroups

>blockGroupCode
> blockGroup
>  =
> case blockGroup of
>  Value'sBlockGroup _ code ->
>   code
>  OtherBlockGroup code ->
>   code

>groupBlocksByValueName ::
> [ProcessedTopLevelBlock] ->
> [Value'sBlockGroup]

>groupBlocksByValueName
> processedBlocks
>  =
> map
>  toBlockGroup
>  $ groupBy
>   areBlocksNamedTheSame
>   processedBlocks

>toBlockGroup ::
> [ProcessedTopLevelBlock] ->
> Value'sBlockGroup

>toBlockGroup
> notYetABlockGroupJustAList
>  =
> case
>   find
>    isProcessedValue
>    notYetABlockGroupJustAList of
>  Just (ProcessedValueDeclaration value _) ->
>   Value'sBlockGroup
>    value
>    (processedTopLevelBlocksCode
>      notYetABlockGroupJustAList)
>  Nothing ->
>   OtherBlockGroup
>    (processedTopLevelBlocksCode
>      notYetABlockGroupJustAList)

>isProcessedValue ::
> ProcessedTopLevelBlock ->
> Bool

>isProcessedValue
> ProcessedValueDeclaration{}
>  =
> True

>isProcessedValue
> _
>  =
> False

>processedTopLevelBlocksCode ::
> [ProcessedTopLevelBlock] ->
> String
>processedTopLevelBlocksCode
> processedTopLevelBlocks
>  =
> concatMap
>  processedTopLevelBlockCode
>  processedTopLevelBlocks

>processedTopLevelBlockCode ::
> ProcessedTopLevelBlock ->
> String
>processedTopLevelBlockCode
> processedTopLevelBlock
>  =
> case processedTopLevelBlock of
>  ProcessedValueDeclaration _ code ->
>   code
>  ProcessedValueTypeDeclaration _ code ->
>   code
>  OtherTypeOfProcessedBlock code ->
>   code

>areBlocksNamedTheSame ::
> ProcessedTopLevelBlock ->
> ProcessedTopLevelBlock ->
> Bool
>areBlocksNamedTheSame
> block1
> block2
>  =
> case (block1,block2) of
>  (ProcessedValueDeclaration value _,
>   ProcessedValueTypeDeclaration typeName _) ->
>    valueName value == typeName
>  (ProcessedValueDeclaration value1 _,
>   ProcessedValueDeclaration value2 _) ->
>    valueName value1 == valueName value2
>  (ProcessedValueTypeDeclaration typeName _,
>   ProcessedValueDeclaration value _) ->
>    valueName value == typeName
>  (_,_) ->
>   False

>expandFunctions ::
> String   ->
> FilePath ->
> Either
>  Error
>  ([Value],
>   Code,
>   Indent)

>expandFunctions
> origionalSource
> fileName = 

> let

We take the Toks generated by her-lexer

>  tokss :: [CodeLine]
>  tokss =
>   fromClutterToLines $
>    ready fileName $
>     unlit fileName origionalSource

>  indentEither :: Either Error Indent
>  indentEither
>    =
>   topIndentationLevel
>    tokss

↓ Incomplete pattern?  No problem.  Lazy evaluation means that our check after the "in" will catch the "Left sided" indentslong before this would ever be looked at. ↓

>  indent :: Indent
>  indent =
>   case indentEither of
>    Right indent' -> indent'

>  blocks :: [CodeBlock]
>  blocks
>    =
>   split
>    (whenElt
>     (\line ->
>        (indentLevel
>          line
>          ==
>        indent)
>         &&
>        (startsWithLid
>          line)))
>    tokss

>  eitherErrorOrProcessedTopLevelBlocks ::
>   Either
>    Error
>    [ProcessedTopLevelBlock]

>  eitherErrorOrProcessedTopLevelBlocks
>    =
>   sequence $
>    map
>     (processBlock
>       indent)
>     blocks

> in
>  case (indentEither,eitherErrorOrProcessedTopLevelBlocks) of
>   (Left error,_) -> Left error
>   (_,Left error) -> Left error
>   (_,Right unfilteredProcessedTopLevelBlocks) ->
>    let
>     processedTopLevelBlocks
>       =
>      filter
>       (not . isEmptyBlock)
>       unfilteredProcessedTopLevelBlocks
>      where
>       isEmptyBlock
>        (OtherTypeOfProcessedBlock
>          "")
>          =
>         True
>       isEmptyBlock
>        _
>          =
>         False

>     blocksGroupedByValueName
>       =
>      groupBlocksByValueName
>       processedTopLevelBlocks
>     values
>       =
>      blockGroupsValues
>       blocksGroupedByValueName
>     code
>       =
>      blockGroupsCode
>       $ commentCertainlyParametricBlocks
>          blocksGroupedByValueName
>    in
>     Right 
>       (values,
>        code,
>        indent)

>startsWithLid ::
> CodeLine ->
> Bool

>startsWithLid 
> ((NL _):(Spc _):(Lid _):_)
>  =
> True

>startsWithLid 
> ((NL _):(Lid _):_)
>  =
> True

>startsWithLid
> _
>  =
> False

>topIndentationLevel ::
> [[Tok]] ->
> -- ^ Toks organized by line.
> Either
>  Error
>  Int

>topIndentationLevel
> (line:tokss)
>  =
> case startsWithLid line of
>  True  ->
>   Right (indentLevel line)
>  False ->
>   topIndentationLevel
>    tokss

>topIndentationLevel
> []
>  =
> Left
>  "Error, no values found, are you sure this is a valid haskarrow file?\n"

>commentCertainlyParametricBlocks ::
> [Value'sBlockGroup] ->
> [Value'sBlockGroup]
>commentCertainlyParametricBlocks
> blockGroups
>  =
> map
>  commentBlockGroupIfParametric
>  blockGroups

>commentBlockGroupIfParametric ::
> Value'sBlockGroup ->
> Value'sBlockGroup

>commentBlockGroupIfParametric
> blockGroup
>  =
> case blockGroup of
>  Value'sBlockGroup
>   value@(Value
>     _
>     _
>     (ValueVariety
>       _
>       CertainlyParametricValue)) code ->
>    (Value'sBlockGroup
>      value
>      ("{-"++code++"-}"))
>  otherBlock ->
>   otherBlock


>processBlock ::
> Indent    ->
> CodeBlock ->
> Either
>  Error
>  ProcessedTopLevelBlock
>processBlock
> indent
> block
>  =
> let
>  longBlock
>    =
>   concat
>    block
> in
> if
>  ((indentLevel
>    longBlock)
>     ==
>   indent)
>     &&
>  (startsWithLid
>    longBlock)
>  then
>   scoopDeclarationAndDependsWithRename
>    longBlock
>  else
>   Right
>    (OtherTypeOfProcessedBlock 
>      $ toksOut longBlock)


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
> Either
>  Error
>  ProcessedTopLevelBlock

>scoopDeclarationAndDependsWithRename
> toks
>   =
> scoopDeclarationAndDepends'
>  toks
>  (\name->
>     name ++
>      valueSuffix)
>  []
>  []
>  True

>scoopDeclarationAndDepends ::
> [Tok] ->
> Either
>  Error
>  ProcessedTopLevelBlock

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
> Either
>  Error
>  ProcessedTopLevelBlock

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
>    (Either
>      Error
>      ProcessedTopLevelBlock)
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
>     Left (ValueDeclaration (ValueVariety
>       Evaluated
>       MaybeParametricValue))
>    "<?" ->
>     Left (ValueDeclaration (ValueVariety
>       Evaluated
>       CertainlyParametricValue))
>    "=" ->
>     Left (ValueDeclaration (ValueVariety
>       Static
>       InternalValue))
>    "=?=" ->
>     Left (ValueDeclaration (ValueVariety
>       Static
>       MaybeParametricValue))
>    "=?" ->
>     Left (ValueDeclaration (ValueVariety
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
>   Left
>    (ValueDeclaration valueVariety@
>     (ValueVariety
>       _
>       isParameter)) ->
>    let
>     (name:depends)
>       =
>      nameAndDepends

>    in
>     Right
>      (ProcessedValueDeclaration
>       (Value
>         name
>         depends
>         valueVariety)
>       $ toksOut 
>         $ processedToks ++ ((Sym "="):toks))

>   Left ValueTypeDeclaration ->
>     Right
>      (ProcessedValueTypeDeclaration
>        (head
>          nameAndDepends)
>        (toksOut
>          $ processedToks ++ ((Sym sym):toks)))

>   Right processedBlock ->
>    processedBlock 

>   Left SymbolErrorInDeclaration ->
>    Left
>     ("Error , unexpected symbol in value declaration:\n"++
>      (toksOut ((Sym sym):toks))++
>      "\nThe symble was:\n"++ sym)

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
> Left
>  ("Block was neither a value declaration nor a value's type declaration:"++(toksOut processedToks))

>indentLevel ::
> [Tok] ->
> Indent

>indentLevel
> (nl@(NL _):spc@(Spc indent):toks)
>  =
> length indent

>indentLevel (nl@(NL _):toks) = 0
>indentLevel [] = 0
