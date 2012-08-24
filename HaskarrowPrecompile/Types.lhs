>module HaskarrowPrecompile.Types where

>data Value = Value {
> valueName    :: String  ,
> valueDepends :: [String],
> valueVariety :: ValueVariety} deriving (Show)

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

>type Dependency = String
>type ValueName  = String

>type Error = String
