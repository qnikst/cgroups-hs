{-# LANGUAGE OverloadedStrings #-}
-- | Module: System.Linux.Cgroups.CpuAcct
--   Author: Alexander Vershilov <alexander.vershilov@gmail.com>
--   License: BSD
--
--   The CPU accounting controller is used to group tasks using cgroups and
--   account the CPU usage of these groups of tasks.
--
--   The CPU accounting controller supports multi-hierarchy groups. An accounting
--   group accumulates the CPU usage of all of its child groups and the tasks
--   directly present in its group.
--
--
-- cpuacct controller uses percpu_counter interface to collect user and
-- system times. This has two side effects:
--
--   *  It is theoretically possible to see wrong values for user and system times.
--      This is because percpu_counter_read() on 32bit systems isn't safe
--      against concurrent writes.
--   *  It is possible to see slightly outdated values for user and system times
--       due to the batch processing nature of percpu_counter.
--
module System.Linux.Cgroups.Subsystem.CpuAcct
  ( subCpuAcct
  , CpuStat(..)
  , CpuUsage(..)
  , CpuUsagePerCpu(..)
  )
  where

import BasicPrelude
import Data.Int
import Data.Default

import System.Linux.Cgroups.Types

subCpuAcct = Controller "cpuAcct"

type UserHZ = Int64

-- | CPU time obtained by the cgroup into user and system times. Currently
-- the following statistics are supported:
--
-- user: Time spent by tasks of the cgroup in user mode.
-- system: Time spent by tasks of the cgroup in kernel mode.
-- 
data CpuStat = CpuStat { cpuStatUser::UserHZ, cpuStatSystem::UserHZ} 
              deriving (Eq,Show)

instance Default CpuStat where
  def = CpuStat 0 0

instance CgroupValue CpuStat where
  subsystem _ = subCpuAcct 
  param _     = "stat"


instance CgroupRead CpuStat where
  unprint = fromLines specCpuSet


specCpuSet :: [Spec CpuStat]
specCpuSet = [ ("user", \x (l:_) -> x{cpuStatUser = read l})
             , ("system", \x (l:_) -> x{cpuStatSystem = read l})
             ]


-- | cpuacct.usage gives the CPU time (in nanoseconds) obtained
-- by this group which is essentially the CPU time obtained by all the tasks
-- in the system.
newtype CpuUsage = CpuUsage {unCpuUsage :: Int64 }
                 deriving (Eq, Show)

instance CgroupValue CpuUsage where
  subsystem _ = subCpuAcct
  param _     = "usage"

instance CgroupRead CpuUsage where
  unprint     = CpuUsage . read

newtype CpuUsagePerCpu = CpuUsagePerCpu {unCpuUsagePerCpu :: [Int64]}

instance CgroupValue CpuUsagePerCpu where
  subsystem _ = subCpuAcct
  param _     = "usage_percpu"

instance CgroupRead CpuUsagePerCpu where
  unprint = CpuUsagePerCpu . map read . words


