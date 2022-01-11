{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HW3.Pretty where
import HW3.Base (HiValue (..), HiFun (..), HiAction (..))
import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.Sequence (Seq)
import Data.Foldable (toList, Foldable (fold))
import Data.ByteString (ByteString, unpack)
import Text.Printf (printf)
import Data.Word (Word8)
import Data.Time (UTCTime)

instance Pretty Rational where pretty = unsafeViaShow

instance Pretty UTCTime where pretty = unsafeViaShow

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
    HiFunRead  -> "read"
    HiFunWrite -> "write"
    HiFunMkDir -> "mkdir"
    HiFunChDir -> "cd"
    HiFunParseTime -> "parse-time"
    HiFunRand -> "rand"

instance Pretty HiAction where
  pretty (HiActionRead  x) = fold [pretty HiFunMkDir, pretty "(", pretty $ show x, pretty ")"]
  pretty (HiActionWrite x y) = fold [pretty HiFunWrite, pretty "(", pretty $ show x, pretty ", ", pretty $ show y, pretty ")"]
  pretty (HiActionMkDir x) = fold [pretty HiFunMkDir, pretty "(", pretty $ show x, pretty ")"]
  pretty (HiActionChDir x) = fold [pretty HiFunChDir, pretty "(", pretty $ show x, pretty ")"]
  pretty HiActionCwd = pretty "cwd"
  pretty HiActionNow = pretty "now"
  pretty (HiActionRand x y) = fold [pretty HiFunRand, pretty "(", pretty x, pretty ", ", pretty y, pretty ")"]

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
  pretty (HiValueBool x) = if x then pretty "true" else pretty "false"
  pretty HiValueNull = pretty "null"
  pretty (HiValueString x) = pretty $ show x
  pretty (HiValueList x) = pretty x
  pretty (HiValueBytes x) = pretty x
  pretty (HiValueAction x) = pretty x
  pretty (HiValueTime x) = fold [pretty HiFunParseTime, pretty "(\"", pretty x, pretty "\")"]

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty
