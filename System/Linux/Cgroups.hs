{-# LANGUAGE OverloadedStrings #-}
-- | Cgroup support basic managing of linux cgroup controllers
--
module System.Linux.Cgroups
  ( cgroupsSupported
  -- * Find cgroup utilities
  -- ** Get all subsystems information
  , lookupCgroupRoot
  --, procCgroups
  --, selfCgroups
  -- * Lookup functions
  -- $lookup
  -- ** lookup subsystems
  , lookupSubsystemRoot
  , lookupSubsystemSelf
  --, lookupProcRoot
  --, lookupSelfRoot
  -- ** Unsafe creation
  , unsafeSubsystemRoot
  -- ** Cgroup movement
  -- *** creation
  , unsafeCgroup
  -- *** appending
  , (</>)
  , (<//>)
  -- * Cgroup manipulation
  , tasksFile
  -- $cgroup_files
  -- ** Creating and moving processes
  , mkCgroup
  , moveTasksTo
  , moveProcsTo
  -- ** Getters
  , getTasks
  , getTasks'
  , getProcs
  , getProcs'
  -- * Controllers
  , subFreezer,subBlkio,subCpuSet
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
import System.Linux.Cgroups.Subsystem.Memory  as EXP
import System.Linux.Cgroups.Subsystem.Devices as EXP

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

-- | Find a root for cgroup 'Subsystem'
-- Internaly uses read of \/proc\/mounts
lookupSubsystemRoot :: Subsystem -> IO (Maybe (Hierarchy Absolute))
lookupSubsystemRoot name = do
  x <- matchMounts (on1st $ (==) (mntName name))
  case x of
    Nothing -> return Nothing
    Just ys -> return $ Hierarchy name <$> (Cgroup . fromText) <$> ith 1 ys

-- | Manually set 'Subsystem's root
unsafeSubsystemRoot :: Subsystem -> FilePath -> Hierarchy Absolute
unsafeSubsystemRoot name fp = Hierarchy name (Cgroup fp)


-- | Manually create Cgroup 
-- May be used to create cgroup value from user input
unsafeCgroup :: FilePath -> Cgroup Absolute
unsafeCgroup fp = Cgroup fp

-- | Lookup 'Subsystem' root of the current process
lookupSubsystemSelf :: Subsystem -> IO (Maybe (Hierarchy Relative))
lookupSubsystemSelf name = do
  ts <- readTextFile "/proc/self/cgroup"
  let r = find (on2nd $ (==) (cgName name)) . (map (T.split (== ':'))) . lines $ ts
  case r of
    Nothing -> return Nothing
    Just xs -> return $ Hierarchy name <$> (Cgroup . fromText . (T.drop 1)) <$> ith 2 xs

{- cgroups movements -}

-- | Append hierarchy to another one
(</>) :: Hierarchy a -> Text -> Hierarchy a
(Hierarchy n f) </> t = Hierarchy n (Cgroup $! unCgroup f F.</> fromText t)

-- | Append relative path to absolute one
(<//>) :: Hierarchy Absolute -> Hierarchy a -> Hierarchy Absolute
(Hierarchy n (Cgroup f1)) <//> (Hierarchy n2 (Cgroup f2)) | n == n2 = Hierarchy n (Cgroup $ f1 F.</> f2)
                                                          | otherwise = error "submodule doesn't match"

-- | Create new cgroup
-- TODO: fix text value
mkCgroup :: (HasCgroup a) =>  a -> Text -> IO a
mkCgroup a t = do
  createDirectory True p'
  return $ acgroup a p'
  where p' = cgroup a F.</> fromText t

-- | move task to specified 'Hierarchy'
moveTasksTo :: (HasCgroup a) => a -> Int -> IO ()
moveTasksTo = move_to "tasks"

-- | move task and all processes in it's group to 'Hierarchy'
moveProcsTo :: (HasCgroup a) => a -> Int -> IO ()
moveProcsTo = move_to "cgroup.procs"

{-# INLINE move_to #-}
move_to :: HasCgroup a => FilePath -> a -> Int -> IO ()
move_to f a t = appendTextFile (cgroup a F.</> f) (show t)

getTasks, getProcs :: HasCgroup a => a -> IO (Set Int)
getTasks h = S.fromList <$> getTasks' h
getProcs h = S.fromList <$> getProcs' h 
getTasks', getProcs' :: HasCgroup a => a -> IO [Int]
getTasks' = int_from "tasks" 
getProcs' = int_from "cgroup.procs"

{-# INLINE int_from #-}
int_from :: HasCgroup a => FilePath -> a -> IO [Int]
int_from f p = map read . lines <$> readTextFile (cgroup p F.</> "tasks") 


subFreezer,subBlkio,subCpuSet :: Subsystem
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

-- $cgroup_files
-- Each cgroup is represented by a directory in the cgroup file system
-- containing the following files describing that cgroup:
--
--  [@tasks@] list of tasks (by PID) attached to that cgroup.  This list
--     is not guaranteed to be sorted.  Writing a thread ID into this file
--     moves the thread into this cgroup.
--
--  [@cgroup.procs@] list of thread group IDs in the cgroup.  This list is
--     not guaranteed to be sorted or free of duplicate TGIDs, and userspace
--     should sort/uniquify the list if this property is required.
--     Writing a thread group ID into this file moves all threads in that
--     group into this cgroup.
--
-- [@notify_on_release@] flag: run the release agent on exit?
--
-- [@release_agent@] the path to use for release notifications (this file
--     exists in the top cgroup only)
--

-- $lookup
-- Each 'Hierarchy' has it's root that basically is \/sys\/fs\/cgroups
-- and it's relavite paths that are listed in \/proc\/X\/cgroups
-- So all Hierarchies are either Absolute or Relative, to perform an action
-- on cgroup you need to use Absolute path.
--
tasksFile :: (HasCgroup a) => a -> FilePath
tasksFile h = cgroup h F.</> "tasks"
