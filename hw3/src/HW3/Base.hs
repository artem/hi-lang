{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module HW3.Base where
import Data.Text ( Text )
import Data.Sequence (Seq)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Codec.Serialise (Serialise)

data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  deriving (Eq, Ord, Show, Generic, Serialise)

data HiValue =
    HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  deriving (Eq, Ord, Show, Generic, Serialise)

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

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
