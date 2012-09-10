>{-# LANGUAGE GADTs,EmptyDataDecls #-}

>module Language.Haskarrow.Types where

>data Value depsResolved influencesResolved = Value {
> valueName          :: String    ,
> valueDepends       :: Depends depsResolved  ,
> listenerInfluences :: Influences influencesResolved,
> -- ^ This property is ignored unless the "EvaluationType" is LoudObject.  However, we include it here, instead of within the evaluation type itself, because when we are parsing, we find the influences before we find out the evaluation type.
> valueVariety       :: ValueVariety}

>data Unresolved
>data Resolved

>data Depends a where
> ResolvedDepends     ::
>  {resolvedDepends     :: [String]}   -> Depends Resolved
> UnresolvedDepends   ::
>  {unresolvedDepends   :: [String]} -> Depends Unresolved

>data Influences a where
> ResolvedInfluences ::
>  {resolvedInfluences :: [Influence]} -> Influences Resolved
> UnresolvedInfluences ::
>  {unresolvedInfluences :: [String]} -> Influences Unresolved

>sourceName
>  ::
> Influence ->
> String
>sourceName
> (Influence (Source (_,name)) _)
>  =
> name

>data Influence = Influence {
>  source              :: Source,
>  dependentInfluences :: [String]
> }

>newtype Source = Source (SourceID,String)

>newtype SourceID = SourceID Int

>isCertainlyParametric ::
> Value a a ->
> Bool
>isCertainlyParametric
> value
>  =
>  CertainlyParametricValue
>             ==
>  (isParameter . valueVariety)
>    value

>data ValueVariety =
> ValueVariety{
>  evaluationType   :: EvaluationType,
>  timeOfEvaluation :: TimeOfEvaluation,
>  isParameter      :: IsParameter}
>  deriving(Show,Eq)

> -- | EvaluationType refers to the way values are evaluated.
>data EvaluationType = Evaluated | Static
> deriving (Show,Eq)

> -- | EvaluationStyle refers to the way the init sequence is evaluated.
>data EvaluationStyle = Concurrent | Sequential

>data TimeOfEvaluation =
>   DeReEval
> | DeReReEval
> deriving (Show,Eq)

>data IsParameter =
> InternalValue        |
> MaybeParametricValue |
> CertainlyParametricValue
>  deriving (Show,Eq)

>type Dependency = String
>type ValueName  = String

>type Error = String

>newtype Indentation = Indentation String deriving (Eq)

>type Code=String
