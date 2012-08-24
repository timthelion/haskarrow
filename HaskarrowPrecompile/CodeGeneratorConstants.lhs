>module HaskarrowPrecompile.CodeGeneratorConstants where

Haskarrow code preprocesses/precompiles to Haskell code.  In this process, all values are renamed, adding valueSuffix to the end:

>valueSuffix = "Value_'_"

A main type+data constructor is then created, named "Values" which will hold the post evaluation/post deparameterized values of all the values in the module.

>valuesDataConstructorName = "Values"

Within this data constructor, there are many records.  One record coresponds to one value.  Each value's name is suffixed with the string:

>recordSuffix = "Value"

There are two more type+data constructors created:

>requiredParametersDataConstructorName = "RequiredParameters"

and

>optionalParametersDataConstructorName = "OptionalParameters"

There is a corresponding:

>optionalParametersEmpty = "emptyOptionalParameters"

Which is filled with Nothing for easy use...

These data constructos contain records that correspond to values with can be changed parametrically.  These records are suffixed:

>parameterSuffix = "Parameter"

A function named "initValues" of type "RequiredParameters aType_'_ bType_'_ cType_'_ ... -> OptionalParameters dType_'_ eType_'_ fType_'_ ... -> IO Values aType_'_ bType_'_ cType_'_ dType_'_ eType_'_ fType_'_ ..." or "RequiredParameters aType_'_ bType_'_ cType_'_ ... -> OptionalParameters dType_'_ eType_'_ fType_'_ ... -> Values aType_'_ bType_'_ cType_'_ dType_'_ eType_'_ fType_'_ ..." depending on if there are any evaluated values in the haskarrow module or not is created.

>internalValueTypeSuffix = "Type_'_"

>initValuesFunctionName = "initValues"

The initValues function evaluates the EvaluatedValues in the haskarrow module in the order determined by dependency resolution like in the example bellow.

initValues :: IO Values
initValues = do
 foo <- fooValue
 bar <- barValue
 foobar <- foobarValue foo bar
 

A main statement is also generated if possible.  See "mainCode" bellow.

