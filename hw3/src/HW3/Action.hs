module HW3.Action where
import Data.Set (Set, member)
import Control.Exception (Exception)
import HW3.Base (HiMonad, runAction, HiAction (..), HiValue (..))
import qualified Control.Monad
import Control.Monad (liftM)
import GHC.IO (throwIO)
import System.Directory
import Data.Text (pack)
import Data.ByteString (writeFile, readFile)
import Prelude hiding (readFile, writeFile)
import Data.Text.Encoding (decodeUtf8')
import Data.Sequence (fromList)

data HiPermission =
    AllowRead
  | AllowWrite
  deriving (Eq, Ord, Show)

data PermissionException =
  PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap = liftM

instance Applicative HIO where
  pure x = HIO (\_ -> return x)
  (<*>) = Control.Monad.ap

instance Monad HIO where
  (HIO x) >>= f = HIO $ \perm ->
    x perm >>= \a -> runHIO (f a) perm

verifyAndRun :: HiPermission -> IO a -> HIO a
verifyAndRun needed h = HIO $ \perm -> if member needed perm then h else throwIO (PermissionRequired needed)

instance HiMonad HIO where
  runAction HiActionCwd = verifyAndRun AllowRead (HiValueString <$> (pack <$> getCurrentDirectory))
  runAction (HiActionChDir x) = verifyAndRun AllowRead (HiValueNull <$ setCurrentDirectory x)
  runAction (HiActionRead x) = verifyAndRun AllowRead (doReadFile x)
  runAction (HiActionWrite x y) = verifyAndRun AllowWrite (HiValueNull <$ writeFile x y)
  runAction (HiActionMkDir x) = verifyAndRun AllowWrite (HiValueNull <$ createDirectory x)

doReadFile :: FilePath -> IO HiValue
doReadFile path = do
  chk <- doesDirectoryExist path
  if not chk then do
    bytes <- readFile path
    let txt = decodeUtf8' bytes
    return $ case txt of
      Left _ -> HiValueBytes bytes
      Right x -> HiValueString x
  else do
    lst <- getDirectoryContents path
    let lst' = fromList $ map (HiValueString . Data.Text.pack) lst
    return $ HiValueList lst'
