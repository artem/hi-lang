module HW3.Action where
import Data.Set (Set)
import Control.Exception (Exception)

data HiPermission =
    AllowRead
  | AllowWrite
  deriving Show

data PermissionException =
  PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
