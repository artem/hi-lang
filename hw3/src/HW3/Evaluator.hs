{-# LANGUAGE OverloadedStrings #-}

module HW3.Evaluator where
import HW3.Base (HiError (..), HiValue (..), HiExpr (..), HiFun (..))
import Control.Monad ((>=>), foldM)
import qualified Data.Text as T
import Data.Semigroup (stimes)
import Data.Ratio (denominator, numerator)
import Data.Sequence (fromList, Seq (..), (><), reverse, lookup, take, drop)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Control.Applicative ((<|>))
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.ByteString (pack, unpack, append)
import Data.Word (Word8)
import Data.Foldable (toList)
import Codec.Compression.Zlib (compressWith, bestCompression, CompressParams (compressLevel), defaultCompressParams, decompress)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Codec.Serialise (serialise, deserialise)

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval = return . evalInEitherMonad

evalInEitherMonad :: HiExpr -> Either HiError HiValue
evalInEitherMonad (HiExprValue x) = Right x
evalInEitherMonad (HiExprApply fexpr args) = do
    xxx <- evalInEitherMonad fexpr
    args <- evalListInEitherMonad args
    apply xxx args

apply :: HiValue -> [HiValue] -> Either HiError HiValue
apply (HiValueFunction fun) = case fun of
   HiFunDiv -> doDiv
   HiFunMul -> doMul
   HiFunAdd -> doPlus
   HiFunSub -> doSub
   HiFunNot -> doNot
   HiFunAnd -> doAnd
   HiFunOr -> doOr
   HiFunLessThan -> doComp LT
   HiFunGreaterThan -> doComp GT
   HiFunEquals -> doComp EQ
   HiFunNotLessThan -> doComp LT >=> doNot . (:[])
   HiFunNotGreaterThan -> doComp GT >=> doNot . (:[])
   HiFunNotEquals -> doComp EQ >=> doNot . (:[])
   HiFunIf -> doIf
   HiFunLength -> doLen
   HiFunToUpper -> doStrFun T.toUpper
   HiFunToLower -> doStrFun T.toLower
   HiFunReverse -> doReverse
   HiFunTrim -> doStrFun T.strip
   HiFunList -> doList
   HiFunRange -> doRange
   HiFunFold -> doInitFold
   HiFunPackBytes -> doPack
   HiFunUnpackBytes -> doUnpack
   HiFunEncodeUtf8 -> doEnUtf8
   HiFunDecodeUtf8 -> doDeUtf8
   HiFunZip -> doZip
   HiFunUnzip -> doUnzip
   HiFunSerialise -> doSerialise
   HiFunDeserialise -> doDeserialise
apply (HiValueString s) = applyStr s
apply (HiValueList l) = applyList l
apply _ = const $ Left HiErrorInvalidFunction

doDeserialise :: [HiValue] -> Either HiError HiValue
doDeserialise [HiValueBytes a] = return $ deserialise $ fromStrict a
doDeserialise l = arityErr 1 l

doSerialise :: [HiValue] -> Either HiError HiValue
doSerialise [x] = return $ HiValueBytes $ toStrict $ serialise x
doSerialise l = arityErr 1 l

doUnzip :: [HiValue] -> Either HiError HiValue
doUnzip [HiValueBytes a] = return $ HiValueBytes $ toStrict $ decompress (fromStrict a)
doUnzip l = arityErr 1 l

doZip :: [HiValue] -> Either HiError HiValue
doZip [HiValueBytes a] = return $ HiValueBytes $ toStrict $ compressWith defaultCompressParams { compressLevel = bestCompression } (fromStrict a)
doZip l = arityErr 1 l

doUnpack :: [HiValue] -> Either HiError HiValue
doUnpack [HiValueBytes a] = return $ HiValueList $ fromList $ map (HiValueNumber . toRational) $ unpack a
doUnpack l = arityErr 1 l

doPack :: [HiValue] -> Either HiError HiValue
doPack [HiValueList l] = mapM intsToWord8s l <&> HiValueBytes . pack . toList
doPack l = arityErr 1 l

intsToWord8s :: HiValue -> Either HiError Word8
intsToWord8s (HiValueNumber x) = safeToInt x >>= \n ->
    if n < 256
    then return $ fromIntegral n
    else Left HiErrorInvalidArgument
intsToWord8s _ = Left HiErrorInvalidArgument

applyBin :: HiValue -> HiValue -> HiValue -> Either HiError HiValue
applyBin f x y = apply f [x, y]

doInitFold :: [HiValue] -> Either HiError HiValue
doInitFold [f, HiValueList (x :<| (y :<| rest))] = apply f [x, y] >>= \acc -> foldM (applyBin f) acc rest
doInitFold l = arityErr 2 l

doList :: [HiValue] -> Either HiError HiValue
doList = return . HiValueList . fromList

doRange :: [HiValue] -> Either HiError HiValue
doRange [HiValueNumber a, HiValueNumber b] = doList $ map HiValueNumber [a..b]
doRange l = arityErr 2 l

safeToInt :: Rational -> Either HiError Int
safeToInt x
  | not (isInt x) = Left HiErrorInvalidArgument
  | otherwise = return $ fromInteger $ numerator x

sliceStr :: Int -> Int -> T.Text -> T.Text
sliceStr x y = if x >= 0
    then
        if y >= 0 then
            (T.drop x) . (T.take y)
        else
            (T.drop x) . (T.dropEnd (-y))
    else
        if y > 0 then
            (\s -> T.drop (x + T.length s) (T.take y s))
        else
            T.takeEnd (y-x) . (T.dropEnd (-y))


leftSlice :: Int -> T.Text -> T.Text
leftSlice x = if x < 0 then T.takeEnd (-x) else T.drop x

applyStr :: T.Text -> [HiValue] -> Either HiError HiValue
applyStr t [HiValueNumber a] = do
    x <- safeToInt a
    let s = T.unpack t
    if x < 0 || x >= length s then return HiValueNull
    else return $ HiValueString $ T.singleton $ s !! x
applyStr t [HiValueNumber a, HiValueNumber b] = do
    x <- safeToInt a
    y <- safeToInt b
    return $ HiValueString $ sliceStr x y t
applyStr t [HiValueNumber a, HiValueNull] = do
    x <- safeToInt a
    return $ HiValueString $ leftSlice x t
applyStr t [HiValueNull, HiValueNumber b] = do
    y <- safeToInt b
    return $ HiValueString $ sliceStr 0 y t
applyStr t [HiValueNull, HiValueNull] = return $ HiValueString t
applyStr _ l = arityErr 2 l

applyList :: Seq HiValue -> [HiValue] -> Either HiError HiValue
applyList l [HiValueNumber a] = do
    x <- safeToInt a
    return $ fromMaybe HiValueNull (Data.Sequence.lookup x l)
applyList l [HiValueNumber a, HiValueNumber b] = do
    x <- safeToInt a
    y <- safeToInt b
    return $ HiValueList $ sliceList x y l
applyList l [HiValueNumber a, HiValueNull] = do
    x <- safeToInt a
    return $ HiValueList $ sliceList x (length l) l
applyList l [HiValueNull, HiValueNumber b] = do
    y <- safeToInt b
    return $ HiValueList $ sliceList 0 y l
applyList l [HiValueNull, HiValueNull] = return $ HiValueList l
applyList _ l = arityErr 2 l

sliceList :: Int -> Int -> Seq HiValue -> Seq HiValue
sliceList a b l = Data.Sequence.take (y - x) (Data.Sequence.drop x l)
    where
        norm x = if x < 0 then x + length l else x
        x = norm a
        y = norm b

doLen :: [HiValue] -> Either HiError HiValue
doLen [HiValueString s] = return $ HiValueNumber $ fromIntegral $ T.length s
doLen [HiValueList l] = return $ HiValueNumber $ fromIntegral $ length l
doLen l = arityErr 1 l

doStrFun :: (T.Text -> T.Text) -> [HiValue] -> Either HiError HiValue
doStrFun f [HiValueString s] = return $ HiValueString $ f s
doStrFun _ l = arityErr 1 l

doDeUtf8 :: [HiValue] -> Either HiError HiValue
doDeUtf8 [HiValueBytes a] = return $ fromRight HiValueNull (decodeUtf8' a <&> HiValueString)
doDeUtf8 l = arityErr 1 l

doEnUtf8 :: [HiValue] -> Either HiError HiValue
doEnUtf8 [HiValueString s] = return $ HiValueBytes $ encodeUtf8 s
doEnUtf8 l = arityErr 1 l

doReverse :: [HiValue] -> Either HiError HiValue
doReverse t@[HiValueString s] = doStrFun T.reverse t
doReverse [HiValueList l] = return $ HiValueList $ Data.Sequence.reverse l
doReverse l = arityErr 1 l

-- doSemiFun :: (forall x. (Semigroup x) => x -> x) -> [HiValue] -> Either HiError HiValue
-- doSemiFun f [HiValueString s] = return $ HiValueString $ f s
-- doSemiFun f [HiValueList s] = return $ HiValueList $ f s
-- doSemiFun _ l = arityErr 1 l

arityErr :: Int -> [HiValue] -> Either HiError HiValue
arityErr x l = if length l /= x then Left HiErrorArityMismatch else Left HiErrorInvalidArgument

doAnd :: [HiValue] -> Either HiError HiValue
doAnd [HiValueBool a, HiValueBool b] = return $ HiValueBool $ a && b
doAnd l = arityErr 2 l

doOr :: [HiValue] -> Either HiError HiValue
doOr [HiValueBool a, HiValueBool b] = return $ HiValueBool $ a || b
doOr l = arityErr 2 l

doComp :: Ordering -> [HiValue] -> Either HiError HiValue
doComp exp [a, b] = return $ HiValueBool $ compare a b == exp
doComp _ l = arityErr 2 l

doNot :: [HiValue] -> Either HiError HiValue
doNot [HiValueBool x] = return $ HiValueBool $ not x
doNot l = arityErr 1 l

doIf :: [HiValue] -> Either HiError HiValue
doIf [HiValueBool cond, a, b] = return $ if cond then a else b
doIf l = arityErr 3 l

doPlus :: [HiValue] -> Either HiError HiValue
doPlus [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a + b)
doPlus [HiValueString a, HiValueString b] = return $ HiValueString (T.append a b)
doPlus [HiValueList a, HiValueList b] = return $ HiValueList (a >< b)
doPlus [HiValueBytes a, HiValueBytes b] = return $ HiValueBytes (append a b)
doPlus l = arityErr 2 l

isInt :: Rational -> Bool
isInt x = denominator x == 1

semiMul :: Semigroup t => (t -> HiValue) -> t -> Rational -> Either HiError HiValue
semiMul f a b
  | b <= 0 = Left HiErrorInvalidArgument
  | otherwise = safeToInt b >>= \x -> return $ f (stimes x a)

doMul :: [HiValue] -> Either HiError HiValue
doMul [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a * b)
doMul [HiValueString a, HiValueNumber b] = semiMul HiValueString a b
doMul [HiValueList a, HiValueNumber b] = semiMul HiValueList a b
doMul [HiValueBytes a, HiValueNumber b] = semiMul HiValueBytes a b
doMul l = arityErr 2 l

doSub :: [HiValue] -> Either HiError HiValue
doSub [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a - b)
doSub l = arityErr 2 l

doDiv :: [HiValue] -> Either HiError HiValue
doDiv [HiValueNumber a, HiValueNumber b]
  | b /= 0 = return $ HiValueNumber (a / b)
  | otherwise = Left HiErrorDivideByZero
doDiv [HiValueString a, HiValueString b] = return $ HiValueString (T.intercalate "/" [a, b])
doDiv l = arityErr 2 l

evalListInEitherMonad :: [HiExpr] -> Either HiError [HiValue]
evalListInEitherMonad = mapM evalInEitherMonad
