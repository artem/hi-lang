module HW3.Evaluator where
import HW3.Base (HiError (..), HiValue (..), HiExpr (..), HiFun (..))
import Control.Monad ((>=>))

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
   HiFunMul -> doNumOp (\a b -> return $ HiValueNumber (a * b))
   HiFunAdd -> doNumOp (\a b -> return $ HiValueNumber (a + b))
   HiFunSub -> doNumOp (\a b -> return $ HiValueNumber (a - b))
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

apply _ = const $ Left HiErrorInvalidFunction

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

doNumOp :: (Rational -> Rational -> Either HiError HiValue) -> [HiValue] -> Either HiError HiValue
doNumOp f [HiValueNumber a, HiValueNumber b] = f a b
doNumOp _ l = arityErr 2 l

doDiv :: [HiValue] -> Either HiError HiValue
doDiv = doNumOp $ \a b -> if b /= 0
    then return $ HiValueNumber (a / b)
    else Left HiErrorDivideByZero

evalListInEitherMonad :: [HiExpr] -> Either HiError [HiValue]
evalListInEitherMonad = mapM evalInEitherMonad
