{-# LANGUAGE EmptyDataDecls #-}
module System.Linux.Cgroups.Types
  ( -- * datatypes
    Cgroup(..)
  , Subsystem(..)
  , Hierarchy(..)
  , Spec
    -- * classes
  , CgroupValue(..)
  , CgroupRead(..)
  , CgroupBox(..)
    -- * helper datatypes
  , Relative
  , Absolute
    -- * functions
  , isNamed
  , mntName
  , fromLines
  )
  where

import BasicPrelude
import Data.Default
import Filesystem
import qualified Filesystem.Path.CurrentOS as F

data Relative
data Absolute

-- | A @cgroup@ associates a set of tasks with a set of parameters for one
-- or more subsystems
newtype Cgroup a = Cgroup {unCgroup::FilePath} deriving (Eq, Show)

-- | A @subsystem@ is a module that makes use of the task grouping
-- facilities provided by cgroups to treat groups of tasks in
-- particular ways. A subsystem is typically a "resource controller" that
-- schedules a resource or applies per-cgroup limits, but it may be
-- anything that wants to act on a group of processes, e.g. a
-- virtualization subsystem.
data Subsystem = Controller Text
               | Named Text
               deriving (Eq, Show)

mntName :: Subsystem -> Text
mntName (Controller t) = t
mntName (Named t) = t

isNamed :: Subsystem -> Bool
isNamed (Named _) = True
isNamed _ = False

-- | A @hierarchy@ is a set of cgroups arranged in a tree, such that
-- every task in the system is in exactly one of the cgroups in the
-- hierarchy, and a set of subsystems; each subsystem has system-specific
-- state attached to each cgroup in the hierarchy.  Each hierarchy has
-- an instance of the cgroup virtual filesystem associated with it.
data Hierarchy a = Hierarchy Subsystem (Cgroup a)
                 deriving (Eq, Show)


class CgroupValue a where
  subsystem :: a -> Subsystem
  param :: a -> Text

class (CgroupValue a) => CgroupRead a where
  unprint :: Text -> a
  get :: Hierarchy Absolute -> IO a
  get (Hierarchy _ (Cgroup f)) = inner undefined
    where inner :: (CgroupRead a) => a -> IO a
          inner hack = unprint <$> readTextFile (f </> F.fromText ssys <.> parm)
            where
              ssys = mntName (subsystem hack)
              parm  = param hack
              {-# INLINE ssys #-}
              {-# INLINE parm #-}
          {-# INLINE inner #-}


class (CgroupValue a, CgroupRead a) => CgroupBox a where
  pprint :: a -> Text 
  set :: Hierarchy Absolute -> a -> IO ()
  modify :: (a -> a) -> Hierarchy Absolute -> IO ()
  -- default realizations
  set (Hierarchy _ (Cgroup f)) v = writeTextFile (f </> (F.fromText . mntName $! subsystem v) <.> (param v)) (pprint v)
  modify f h = get h >>= set h . f

type Spec a = (Text, a -> [Text] -> a)

fromLines :: (Default a) => [Spec a] -> Text -> a
fromLines spec = go def spec . (map words) . lines
  where 
    go x _  [] = x
    go x [] (l:ls) = go x spec ls
    go x _  ([l]:ls) = go x spec ls
    go x (s@(sn,f):sx) l@((ln:lv):ls) | sn == ln = go (f x lv) spec ls
                                      | otherwise = go x sx l
