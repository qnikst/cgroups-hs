{-# LANGUAGE OverloadedStrings #-}
module System.Linux.Cgroups
  ( cgroupsSupported
  -- * Find cgroup utilities
  -- ** Get all subsystems information
  , lookupCgroupRoot
  --, procCgroups
  --, selfCgroups
  -- * Lookup functions
  , lookupSubsystemRoot
  , lookupSubsystemSelf
  --, lookupProcRoot
  --, lookupSelfRoot
  -- ** unsafe creation
  , unsafeSubsystemRoot
  -- * cgroup manipulation
  -- ** cgroup
  , (</>)
  , (<//>)
  -- ** creating and moving processes
  , mkCgroup
  , moveTasksTo
  , moveProcsTo
  -- ** getters
  , getTasks
  , getTasks'
  , getProcs
  , getProcs'
  -- * controllers
  , subMemory,subDevices,subFreezer,subBlkio,subCpuSet
  , module EXP
  ) where

import BasicPrelude hiding ((</>))
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Filesystem
import Filesystem.Path.CurrentOS (fromText)
import qualified Filesystem.Path.CurrentOS as F
import Control.Monad.Trans

import System.Linux.Cgroups.Types
-- export modules
import System.Linux.Cgroups.Subsystem.Cpu     as EXP
import System.Linux.Cgroups.Subsystem.CpuAcct as EXP

-- | Check if cgroups is supported by kernel
cgroupsSupported :: IO Bool
cgroupsSupported = isFile "/proc/cgroups"

-- | Find rootfs for cgroup virtual filesystem
lookupCgroupRoot :: IO (Maybe (Cgroup Absolute))
lookupCgroupRoot = do
  x <- matchMounts (on1st $ (==) "cgroup_root")
  case x of
    Nothing -> return Nothing
    Just ys -> return $ (Cgroup . fromText) <$> ith 1 ys


-- listCgroups :: IO (CgroupName, Int, Int, Bool)
-- listCgroups = readFile "/proc/cgroups"

-- procCgroups :: Int -> IO (CgroupName, FilePath)
-- procCgroups = do
--   readFile ("/proc" </> (fromText . show $ pid) </> "cgroup")

-- | Find a root for cgroup @subsystem@
-- Internaly uses read of /proc/mounts
lookupSubsystemRoot :: Subsystem -> IO (Maybe (Hierarchy Absolute))
lookupSubsystemRoot name = do
  x <- matchMounts (on1st $ (==) (mntName name))
  case x of
    Nothing -> return Nothing
    Just ys -> return $ Hierarchy name <$> (Cgroup . fromText) <$> ith 1 ys

-- | Manually set subsystem root
unsafeSubsystemRoot :: Subsystem -> FilePath -> Hierarchy Absolute
unsafeSubsystemRoot name fp = Hierarchy name (Cgroup fp)
  {- absolute fp ?? -}

-- lookupProcRoot :: SubmoduleName -> IO (Maybe Submodule)

-- | lookup subsystem root of the current process
lookupSubsystemSelf :: Subsystem -> IO (Maybe (Hierarchy Relative))
lookupSubsystemSelf name = do
  ts <- readTextFile "/proc/self/cgroup"
  let r = find (on2nd $ (==) (cgName name)) . (map (T.split (== ':'))) . lines $ ts
  case r of
    Nothing -> return Nothing
    Just xs -> return $ Hierarchy name <$> (Cgroup . fromText . (T.drop 1)) <$> ith 2 xs

{- cgroups movements -}

(</>) :: Hierarchy a -> Text -> Hierarchy a
(Hierarchy n f) </> t = Hierarchy n (Cgroup $! unCgroup f F.</> fromText t)

(<//>) :: Hierarchy Absolute -> Hierarchy a -> Hierarchy Absolute
(Hierarchy n (Cgroup f1)) <//> (Hierarchy n2 (Cgroup f2)) | n == n2 = Hierarchy n (Cgroup $ f1 F.</> f2)
                                                          | otherwise = error "submodule doesn't match"

-- | Create new cgroup
-- TODO: fix text value
mkCgroup :: Hierarchy Absolute -> Text -> IO (Hierarchy Absolute)
mkCgroup (Hierarchy n p) t = do
  createDirectory True p'
  return $ Hierarchy n (Cgroup p')
  where p' = unCgroup p F.</> fromText t

moveTasksTo :: Hierarchy Absolute -> Int -> IO ()
moveTasksTo = move_to "tasks"

moveProcsTo :: Hierarchy Absolute -> Int -> IO ()
moveProcsTo = move_to "cgroup.procs"

{-# INLINE move_to #-}
move_to f (Hierarchy _ p) t = appendTextFile (unCgroup p F.</> f) (show t)

getTasks, getProcs :: Hierarchy Absolute -> IO (Set Int)
getTasks h = S.fromList <$> getTasks' h
getProcs h = S.fromList <$> getProcs' h 
getTasks', getProcs' :: Hierarchy Absolute -> IO [Int]
getTasks' = int_from "tasks" 
getProcs' = int_from "cgroup.procs"

{-# INLINE int_from #-}
int_from f (Hierarchy _ p) = map read . lines <$> readTextFile (unCgroup p F.</> "tasks") 


subMemory,subDevices,subFreezer,subBlkio,subCpuSet :: Subsystem
subMemory = Controller "memory"
subDevices = Controller "devices"
subFreezer = Controller "freezer"
subBlkio = Controller "blkio"
subCpuSet = Controller "cpuset"

ith :: Int -> [a] -> Maybe a
ith _ [] = Nothing
ith 0 (x:_) = Just x
ith n (x:xs) = ith (n-1) xs

on1st :: (a -> Bool) -> [a] -> Bool
on1st _ [] = False
on1st f (x:_) = f x

on2nd :: (a -> Bool) -> [a] -> Bool
on2nd _ [] = False 
on2nd _ [_] = False 
on2nd f (_:x:_) = f x


matchMounts :: ([Text] -> Bool) -> IO (Maybe [Text])
matchMounts f = do
  mts <- readTextFile "/proc/mounts"
  return $! find f . (map words) . lines $ mts 


cgName :: Subsystem -> Text
cgName (Controller t) = t
cgName (Named t) = "name=" ++ t

{-- 
-- Each cgroup is represented by a directory in the cgroup file system
-- containing the following files describing that cgroup:

-- tasks: list of tasks (by PID) attached to that cgroup.  This list
--     is not guaranteed to be sorted.  Writing a thread ID into this file
--     moves the thread into this cgroup.
-- cgroup.procs: list of thread group IDs in the cgroup.  This list is
--     not guaranteed to be sorted or free of duplicate TGIDs, and userspace
--     should sort/uniquify the list if this property is required.
--     Writing a thread group ID into this file moves all threads in that
--     group into this cgroup.
-- notify_on_release flag: run the release agent on exit?
-- release_agent: the path to use for release notifications (this file
--     exists in the top cgroup only)
--}


