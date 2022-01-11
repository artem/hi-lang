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
import Data.Map.Strict (Map, assocs)
import Data.Ratio (numerator)
import GHC.Real (denominator, Ratio ((:%)))
import Data.Scientific (fromRationalRepetendUnlimited)
import Data.Maybe (isNothing)

instance Pretty Rational where
  pretty x
    | denominator x == 1 = pretty $ numerator x
    | isNothing period = pretty $ show conv
    | abs num < denom = fold [pretty num, pretty "/", pretty denom]
    | otherwise = fold [pretty q, pretty $ if num > 0 then " + " else " - ", pretty (abs r), pretty "/", pretty denom]
    where
        (conv, period) = fromRationalRepetendUnlimited x
        (num :% denom) = x
        (q, r) = quotRem num denom

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
    HiFunEcho -> "echo"
    HiFunCount -> "count"
    HiFunKeys -> "keys"
    HiFunValues -> "values"
    HiFunInvert -> "invert"

strHelper :: (Show a) => HiFun -> a -> Doc ann
strHelper f x = fold [pretty f, pretty "(", pretty $ show x, pretty ")"]

instance Pretty HiAction where
  pretty (HiActionRead  x) = strHelper HiFunRead x
  pretty (HiActionWrite x y) = fold [pretty HiFunWrite, pretty "(", pretty $ show x, pretty ", ", pretty $ show y, pretty ")"]
  pretty (HiActionMkDir x) = strHelper HiFunMkDir x
  pretty (HiActionChDir x) = strHelper HiFunChDir x
  pretty HiActionCwd = pretty "cwd"
  pretty HiActionNow = pretty "now"
  pretty (HiActionRand x y) = fold [pretty HiFunRand, pretty "(", pretty x, pretty ", ", pretty y, pretty ")"]
  pretty (HiActionEcho x) = strHelper HiFunEcho x

instance Pretty a => Pretty (Seq a) where
  pretty = pretty . toList

listPretty :: Pretty a1 => String -> String -> a1 -> (a2 -> Doc ann) -> [a2] -> Doc ann
listPretty l r s f = align . blist . map f where
  blist = group . encloseSep (flatAlt (pretty (l ++ " ")) (pretty l))
                             (flatAlt (pretty (' ' : r)) (pretty r))
                             (pretty s)

instance Pretty ByteString where
  pretty x = listPretty "[#" "#]" " " (pretty . (printf "%02x" :: (Word8 -> String))) $ unpack x

instance Pretty a => Pretty (Map a a) where
  pretty x = listPretty "{" "}" ", " (\(a, b) -> pretty a <> pretty ": " <> pretty b) $ assocs x

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
  pretty (HiValueDict x) = pretty x

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty
