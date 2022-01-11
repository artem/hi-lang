module HW3.Action where
import Data.Set (Set, member)
import Control.Exception (Exception)
import HW3.Base (HiMonad, runAction, HiAction (..), HiValue (..))
import qualified Control.Monad
import Control.Monad (liftM)
import GHC.IO (throwIO)
import System.Directory
import Data.Text (pack, unpack)
import Data.ByteString (writeFile, readFile)
import Prelude hiding (readFile, writeFile)
import Data.Text.Encoding (decodeUtf8')
import Data.Sequence (fromList)
import Data.Time (getCurrentTime)
import System.Random (getStdRandom, Random (randomR))

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
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
  runAction HiActionNow = verifyAndRun AllowTime (HiValueTime <$> getCurrentTime)
  runAction (HiActionRand x y) = HIO $ \_ -> HiValueNumber . toRational <$> getStdRandom (randomR (x,y))
  runAction (HiActionEcho x) = verifyAndRun AllowWrite (HiValueNull <$ putStrLn (unpack x))

doReadFile :: FilePath -> IO HiValue
doReadFile path = do
  chkDir <- doesDirectoryExist path
  chkFile <- doesFileExist path
  if chkDir then do
    lst <- listDirectory path
    let lst' = fromList $ map (HiValueString . Data.Text.pack) lst
    return $ HiValueList lst'
  else if chkFile then do
    bytes <- readFile path
    let txt = decodeUtf8' bytes
    return $ case txt of
      Left _ -> HiValueBytes bytes
      Right x -> HiValueString x
  else return HiValueNull
