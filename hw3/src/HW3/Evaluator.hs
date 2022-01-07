module HW3.Evaluator where
import HW3.Base (HiError, HiValue (HiValueNumber, HiValueFunction, HiValueNull), HiExpr (HiExprValue, HiExprApply))
import qualified Control.Monad

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval (HiExprValue x) = return $ Right x
eval (HiExprApply fexpr args) = do
    return $ evalInEitherMonad (HiExprApply fexpr args)

evalInEitherMonad :: HiExpr -> Either a HiValue
evalInEitherMonad (HiExprValue x) = Right x
evalInEitherMonad (HiExprApply fexpr args) = do
    xxx <- evalInEitherMonad fexpr
    args <- evalListInEitherMonad args
    apply xxx args
evalInEitherMonad _ = undefined

apply :: Monad m => HiValue -> [HiValue] -> m HiValue
apply (HiValueFunction fun) lst = do
    return HiValueNull
apply _ _ = undefined

evalListInEitherMonad :: [HiExpr] -> Either a [HiValue]
evalListInEitherMonad = mapM evalInEitherMonad
