{-# LANGUAGE FlexibleInstances #-}

module HW3.Pretty where
import HW3.Base (HiValue (..), HiFun (..))
import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.Sequence (Seq)
import Data.Foldable (toList)

instance Pretty Rational where pretty = unsafeViaShow

instance Pretty HiFun where
  pretty x = pretty $ case x of
    HiFunDiv -> "div"
    HiFunMul -> "mul"
    HiFunAdd -> "add"
    HiFunSub -> "sub"
    HiFunNot -> "not"
    HiFunAnd -> "and"
    HiFunOr -> "or"
    HiFunLessThan -> "less-than"
    HiFunGreaterThan -> "greater-than"
    HiFunEquals -> "equals"
    HiFunNotLessThan -> "not-less-than"
    HiFunNotGreaterThan -> "not-greater-than"
    HiFunNotEquals -> "not-equals"
    HiFunIf -> "if"
    HiFunLength -> "length"
    HiFunToUpper -> "to-upper"
    HiFunToLower -> "to-lower"
    HiFunReverse -> "reverse"
    HiFunTrim -> "trim"
    HiFunList -> "list"
    HiFunRange -> "range"
    HiFunFold -> "fold"

instance Pretty HiValue where
  pretty (HiValueNumber x) = pretty x
  pretty (HiValueFunction x) = pretty x
  pretty (HiValueBool x) = pretty x
  pretty HiValueNull = pretty "null"
  pretty (HiValueString x) = pretty x
  pretty (HiValueList x) = pretty $ toList x

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty
