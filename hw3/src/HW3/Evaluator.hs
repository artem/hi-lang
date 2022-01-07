module HW3.Evaluator where
import HW3.Base (HiError (..), HiValue (..), HiExpr (..), HiFun (..))
import qualified Control.Monad

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
apply _ = const $ Left HiErrorInvalidFunction

doNumOp :: (Rational -> Rational -> Either HiError HiValue) -> [HiValue] -> Either HiError HiValue
doNumOp f [HiValueNumber a, HiValueNumber b] = f a b
doNumOp _ l = if length l /= 2 then Left HiErrorArityMismatch else Left HiErrorInvalidArgument

doDiv :: [HiValue] -> Either HiError HiValue
doDiv = doNumOp $ \a b -> if b /= 0
    then return $ HiValueNumber (a / b)
    else Left HiErrorDivideByZero

evalListInEitherMonad :: [HiExpr] -> Either HiError [HiValue]
evalListInEitherMonad = mapM evalInEitherMonad
