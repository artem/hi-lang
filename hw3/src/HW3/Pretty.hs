{-# LANGUAGE FlexibleInstances #-}

module HW3.Pretty where
import HW3.Base (HiValue (..), HiFun (..))
import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.Sequence (Seq)
import Data.Foldable (toList)
import Data.ByteString (ByteString, unpack)
import Text.Printf (printf)
import Data.Word (Word8)

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
    HiFunPackBytes -> "pack-bytes"
    HiFunUnpackBytes -> "unpack-bytes"
    HiFunEncodeUtf8 -> "encode-utf8"
    HiFunDecodeUtf8 -> "decode-utf8"
    HiFunZip -> "zip"
    HiFunUnzip -> "unzip"
    HiFunSerialise -> "serialise"
    HiFunDeserialise -> "deserialise"

instance Pretty a => Pretty (Seq a) where
  pretty = pretty . toList

instance Pretty ByteString where
  pretty x = (align . blist . map (pretty . (printf "%02x" :: (Word8 -> String)))) $ unpack x where
    blist = group . encloseSep (flatAlt (pretty "[# ") (pretty "[#"))
                          (flatAlt (pretty " #]") (pretty "#]"))
                          (pretty " ")

instance Pretty HiValue where
  pretty (HiValueNumber x) = pretty x
  pretty (HiValueFunction x) = pretty x
  pretty (HiValueBool x) = pretty x
  pretty HiValueNull = pretty "null"
  pretty (HiValueString x) = pretty x
  pretty (HiValueList x) = pretty x
  pretty (HiValueBytes x) = pretty x

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty
