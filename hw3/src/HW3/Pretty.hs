{-# LANGUAGE FlexibleInstances #-}
module HW3.Pretty where
import HW3.Base (HiValue (HiValueNumber))
import Prettyprinter
import Prettyprinter.Render.Terminal

-- instance Pretty (Rational)

instance Pretty Rational where pretty = unsafeViaShow

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber x) = pretty x
