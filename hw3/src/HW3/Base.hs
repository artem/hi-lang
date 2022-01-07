module HW3.Base where
import Data.Text

data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  deriving (Eq, Ord, Show)

data HiValue =
    HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  deriving (Eq, Ord, Show)

data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving (Eq, Ord, Show)

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Eq, Ord, Show)
