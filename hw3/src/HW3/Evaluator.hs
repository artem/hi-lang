{-# LANGUAGE OverloadedStrings #-}

module HW3.Evaluator where
import HW3.Base (HiError (..), HiValue (..), HiExpr (..), HiFun (..), HiMonad (runAction), HiAction (..))
import Control.Monad ((>=>), foldM)
import qualified Data.Text as T
import Data.Semigroup (stimes)
import Data.Ratio (denominator, numerator)
import Data.Sequence (fromList, Seq (..), (><), reverse, lookup, take, drop)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.ByteString (pack, unpack, append, ByteString)
import Data.Word (Word8)
import Data.Foldable (toList, Foldable (foldl'))
import Codec.Compression.Zlib (compressWith, bestCompression, CompressParams (compressLevel), defaultCompressParams, decompress)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Codec.Serialise (serialise, deserialise)
import Text.Read (readMaybe)
import Data.Time (addUTCTime, diffUTCTime)
import Data.Map.Strict (fromList, Map, lookup, empty, lookupMin, deleteMin, insert, elems, keys, adjust, member)
import Control.Monad.Trans.Except (runExceptT, ExceptT, throwE)
import Control.Monad.Trans.Class (lift)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (eval' expr)

eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprRun x) = do
    res <- eval' x
    case res of
         HiValueAction ha -> lift $ runAction ha
         _ -> throwE HiErrorInvalidArgument
eval' (HiExprValue x) = return x
eval' (HiExprApply fexpr args) = do
    f <- eval' fexpr
    evalLazy f args
eval' (HiExprDict x) = HiValueDict . Data.Map.Strict.fromList <$> traverse pairEval x

pairEval :: HiMonad m => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
pairEval (a, b) = do
    x1 <- eval' a
    x2 <- eval' b
    return (x1, x2)

evalLazy :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalLazy (HiValueFunction HiFunIf) [x, a, b] = do
    cond <- eval' x
    case cond of
      (HiValueBool bcond) -> if bcond then eval' a else eval' b
      _ -> throwE HiErrorInvalidArgument
evalLazy (HiValueFunction HiFunAnd) [a, b] = do
    cond <- eval' a
    case cond of
      r@(HiValueBool bcond) -> if bcond then eval' b else return r
      HiValueNull -> return HiValueNull
      _ -> eval' b
evalLazy (HiValueFunction HiFunOr) [a, b] = do
    cond <- eval' a
    case cond of
      r@(HiValueBool bcond) -> if bcond then return r else eval' b
      HiValueNull -> eval' b
      x -> return x
evalLazy hv args = do
          args' <- mapM eval' args
          let res = apply hv args'
          case res of
            Left he -> throwE he
            Right hv' -> return hv'

apply :: HiValue -> [HiValue] -> Either HiError HiValue
apply (HiValueFunction fun) = case fun of
   HiFunDiv -> doDiv
   HiFunMul -> doMul
   HiFunAdd -> doPlus
   HiFunSub -> doSub
   HiFunNot -> doNot
   HiFunLessThan -> doComp LT
   HiFunGreaterThan -> doComp GT
   HiFunEquals -> doComp EQ
   HiFunNotLessThan -> doComp LT >=> doNot . (:[])
   HiFunNotGreaterThan -> doComp GT >=> doNot . (:[])
   HiFunNotEquals -> doComp EQ >=> doNot . (:[])
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
   HiFunRead -> doSingleAction HiActionRead
   HiFunWrite -> doWrite
   HiFunMkDir -> doSingleAction HiActionMkDir
   HiFunChDir -> doSingleAction HiActionChDir
   HiFunParseTime -> doParseTime
   HiFunRand -> doRand
   HiFunEcho -> doEcho
   HiFunCount -> doCount
   HiFunKeys -> doDictKeys
   HiFunValues -> doDictValues
   HiFunInvert -> doInvert
   _ -> const $ Left HiErrorInvalidFunction
apply (HiValueString s) = applyStr s
apply (HiValueList l) = applyList l
apply (HiValueBytes b) = applyList $ unpackHelper b
apply (HiValueDict m) = applyMap m
apply _ = const $ Left HiErrorInvalidFunction

doCount :: [HiValue] -> Either HiError HiValue
doCount [HiValueBytes bytes] = return $ HiValueDict $ countHelper (HiValueNumber . fromIntegral) (unpack bytes)
doCount [HiValueList l] = return $ HiValueDict $ countHelper id l
doCount [HiValueString s] = return $ HiValueDict $ countHelper (HiValueString . T.singleton) (T.unpack s)
doCount l = arityErr 1 l

countHelper :: Foldable t1 => (t2 -> HiValue) -> t1 t2 -> Map HiValue HiValue
countHelper mapEl = foldl' (\l element -> if member (mapEl element) l
                                   then adjust cnt (mapEl element) l
                                   else insert (mapEl element) (HiValueNumber 1) l) empty
                           where
                            cnt (HiValueNumber x) = HiValueNumber $ x + 1
                            cnt _ = error "Cannot increment HiValue"

doDictValues :: [HiValue] -> Either HiError HiValue
doDictValues [HiValueDict x] = return $ HiValueList $ Data.Sequence.fromList $ elems x
doDictValues l = arityErr 1 l

doDictKeys :: [HiValue] -> Either HiError HiValue
doDictKeys [HiValueDict x] = return $ HiValueList $ Data.Sequence.fromList $ keys x
doDictKeys l = arityErr 1 l

flipAL :: (Ord key, Ord val) => Map key val -> Map val [key]
flipAL oldl =
    let worker :: (Ord key, Ord val) => Map key val -> Map val [key] -> Map val [key]
        worker left accum =
            let minElem = lookupMin left in
                case minElem of
                    Nothing -> accum
                    Just (k, v) -> case Data.Map.Strict.lookup v accum of
                                            Nothing -> worker (deleteMin left) (insert v [k] accum)
                                            Just _  -> worker (deleteMin left) (adjust (k:) v accum)
        in
        worker oldl empty

doInvert :: [HiValue] -> Either HiError HiValue
doInvert [HiValueDict x] = return $ HiValueDict $ HiValueList . Data.Sequence.fromList <$> flipAL x
doInvert l = arityErr 1 l

applyMap :: Map HiValue HiValue -> [HiValue] -> Either HiError HiValue
applyMap m [x] = return $ fromMaybe HiValueNull (Data.Map.Strict.lookup x m)
applyMap _ l = arityErr 1 l

doEcho :: [HiValue] -> Either HiError HiValue
doEcho [HiValueString x] = return $ HiValueAction $ HiActionEcho x
doEcho l = arityErr 1 l

doRand :: [HiValue] -> Either HiError HiValue
doRand [HiValueNumber a, HiValueNumber b] =
    safeToInt a >>= \x ->
    safeToInt b >>= \y ->
        return $ HiValueAction $ HiActionRand x y
doRand l = arityErr 2 l

doParseTime :: [HiValue] -> Either HiError HiValue
doParseTime [HiValueString x] = return $ maybe HiValueNull HiValueTime (readMaybe (T.unpack x))
doParseTime l = arityErr 1 l

doSingleAction :: (String -> HiAction) -> [HiValue] -> Either HiError HiValue
doSingleAction f [HiValueString x] = return $ HiValueAction $ f (T.unpack x)
doSingleAction _ l = arityErr 1 l

doWrite :: [HiValue] -> Either HiError HiValue
doWrite [HiValueString x, HiValueString y] = return $ HiValueAction $ HiActionWrite (T.unpack x) (encodeUtf8 y)
doWrite [HiValueString x, HiValueBytes y] = return $ HiValueAction $ HiActionWrite (T.unpack x) y
doWrite l = arityErr 2 l

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

unpackHelper :: ByteString -> Seq HiValue
unpackHelper a = Data.Sequence.fromList $ map (HiValueNumber . toRational) $ unpack a

doUnpack :: [HiValue] -> Either HiError HiValue
doUnpack [HiValueBytes a] = return $ HiValueList $ unpackHelper a
doUnpack l = arityErr 1 l

doPack :: [HiValue] -> Either HiError HiValue
doPack [HiValueList l] = mapM intsToWord8s l <&> HiValueBytes . pack . Data.Foldable.toList
doPack l = arityErr 1 l

intsToWord8s :: HiValue -> Either HiError Word8
intsToWord8s (HiValueNumber x) = safeToInt x >>= \n ->
    if n < 256 && n > 0
    then return $ fromIntegral n
    else Left HiErrorInvalidArgument
intsToWord8s _ = Left HiErrorInvalidArgument

applyBin :: HiValue -> HiValue -> HiValue -> Either HiError HiValue
applyBin f x y = apply f [x, y]

doInitFold :: [HiValue] -> Either HiError HiValue
doInitFold [f, HiValueList (x :<| (y :<| rest))] = apply f [x, y] >>= \acc -> foldM (applyBin f) acc rest
doInitFold [_, HiValueList (y :<| _)] = return y
doInitFold [_, HiValueList _] = return HiValueNull
doInitFold l = arityErr 2 l

doList :: [HiValue] -> Either HiError HiValue
doList = return . HiValueList . Data.Sequence.fromList

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
        norm n = if n < 0 then n + length l else n
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
doReverse t@[HiValueString _] = doStrFun T.reverse t
doReverse [HiValueList l] = return $ HiValueList $ Data.Sequence.reverse l
doReverse l = arityErr 1 l

arityErr :: Int -> [HiValue] -> Either HiError HiValue
arityErr x l = if length l /= x then Left HiErrorArityMismatch else Left HiErrorInvalidArgument

doComp :: Ordering -> [HiValue] -> Either HiError HiValue
doComp p [a, b] = return $ HiValueBool $ compare a b == p
doComp _ l = arityErr 2 l

doNot :: [HiValue] -> Either HiError HiValue
doNot [HiValueBool x] = return $ HiValueBool $ not x
doNot l = arityErr 1 l

doPlus :: [HiValue] -> Either HiError HiValue
doPlus [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber (a + b)
doPlus [HiValueString a, HiValueString b] = return $ HiValueString (T.append a b)
doPlus [HiValueList a, HiValueList b] = return $ HiValueList (a >< b)
doPlus [HiValueBytes a, HiValueBytes b] = return $ HiValueBytes (append a b)
doPlus [HiValueTime a, HiValueNumber b] = return $ HiValueTime (addUTCTime (fromRational b) a)
doPlus l = arityErr 2 l

isInt :: Rational -> Bool
isInt x = denominator x == 1
  && num >= fromIntegral (minBound :: Int)
  && num <= fromIntegral (maxBound :: Int)
  where num = numerator x

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
doSub [HiValueTime a, HiValueTime b] = return $ HiValueNumber $ toRational (diffUTCTime a b)
doSub l = arityErr 2 l

doDiv :: [HiValue] -> Either HiError HiValue
doDiv [HiValueNumber a, HiValueNumber b]
  | b /= 0 = return $ HiValueNumber (a / b)
  | otherwise = Left HiErrorDivideByZero
doDiv [HiValueString a, HiValueString b] = return $ HiValueString (T.intercalate "/" [a, b])
doDiv l = arityErr 2 l
