{-# LANGUAGE FlexibleInstances #-}

module HW3.Pretty where
import HW3.Base (HiValue (..), HiFun (..))
import Prettyprinter
import Prettyprinter.Render.Terminal

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

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber x) = pretty x
prettyValue (HiValueFunction x) = pretty x
prettyValue (HiValueBool x) = pretty x
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString x) = pretty x
