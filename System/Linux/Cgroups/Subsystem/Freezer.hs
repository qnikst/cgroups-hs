{-# LANGUAGE OverloadedStrings #-}
-- | The cgroup freezer is useful to batch job management system which start
-- and stop sets of tasks in order to schedule the resources of a machine
-- according to the desires of a system administrator. This sort of program
-- is often used on HPC clusters to schedule access to the cluster as a
-- whole. The cgroup freezer uses cgroups to describe the set of tasks to
-- be started/stopped by the batch job management system. It also provides
-- a means to start and stop the tasks composing the job.
--
-- The cgroup freezer will also be useful for checkpointing running groups
-- of tasks. The freezer allows the checkpoint code to obtain a consistent
-- image of the tasks by attempting to force the tasks in a cgroup into a
-- quiescent state. Once the tasks are quiescent another task can
-- walk /proc or invoke a kernel interface to gather information about the
-- quiesced tasks. Checkpointed tasks can be restarted later should a
-- recoverable error occur. This also allows the checkpointed tasks to be
-- migrated between nodes in a cluster by copying the gathered information
-- to another node and restarting the tasks there.
--
-- Note freezer.state doesn't exist in root cgroup, which means root cgroup
-- is non-freezable.
--
-- * Examples of usage :
--
-- # mkdir /sys/fs/cgroup/freezer
-- # mount -t cgroup -ofreezer freezer /sys/fs/cgroup/freezer
-- # mkdir /sys/fs/cgroup/freezer/0
-- # echo $some_pid > /sys/fs/cgroup/freezer/0/tasks
--
-- @
-- >>> c0 <- mkCgroup hier "0"
-- >>> (get Hierarchy :: IO FreezerState)
--   THAWED
-- @
--
-- to freeze all tasks in the container :
-- @
-- >>> freeze hier
-- >>> get hier :: IO FreezerState
--   FREEZING
-- >>> get hier :: IO FreezerState
--   FROZEN
-- @
--
-- to unfreeze all tasks in the container :
-- @
-- >>> unfreeze hier
-- >>> get hier :: IO FreezerState
--   THAWED
-- @
--
-- This is the basic mechanism which should do the right thing for user space task
-- in a simple scenario.
--
-- It's important to note that freezing can be incomplete. In that case we return
-- EBUSY. This means that some tasks in the cgroup are busy doing something that
-- prevents us from completely freezing the cgroup at this time. After EBUSY,
-- the cgroup will remain partially frozen -- reflected by freezer.state reporting
-- "FREEZING" when read. The state will remain "FREEZING" until one of these
-- things happens:
--
--   1. Userspace cancels the freezing operation by writing "THAWED" to
--       the freezer.state file
--   2. Userspace retries the freezing operation by writing "FROZEN" to
--       the freezer.state file (writing "FREEZING" is not legal
--       and returns EINVAL)
--   3. The tasks that blocked the cgroup from entering the "FROZEN"
--       state disappear from the cgroup's set of tasks.
--
-- TODO handle errors
module System.Linux.Cgroup.Subsystem.Freezer
  ( subFreezer
  , FreezerState
  , freeze
  , unfreeze
  )
  where

subFreezer = Controller "Freezer"

data FreezerState = Freezing
                  | Frozen
                  | Thawed
                  deriving (Eq, Show)

instance CgroupValue FreezerState
  subsystem _ = subFreezer
  param     _ = "freezer.state"

instace CgroupValueR FreezerState
  unprint "Freezing" = Freezing
  unprint "Frozen"   = Frozen
  unprint "Thawed"   = Thawed

freeze :: Hierarchy Absolute -> IO ()
freeze (Hierarchy _ (Cgroup f)) = writeTextFile (f </> "freezer.state") "FROZEN"

unfreeze :: Hierarchy Absolute -> IO ()
unfreeze (Hierarchy _ (Cgroup f)) = writeTextFile (f </> "freezer.state") "THAWED"


