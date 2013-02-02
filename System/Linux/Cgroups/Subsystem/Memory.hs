{-# LANGUAGE CPP, OverloadedStrings #-}
module System.Linux.Cgroups.Subsystem.Memory
  ( subMemory
  -- * datatypes
  , Stat(..)
  , MemStat(..)
  -- * cgroup values
  -- ** controls
  , ForceEmpty(..)
  , OOMControl(..)
  , UseHierarchy(..)
  , Swappines(..)
  , MoveChargeAtMigrate(..)
  -- ** memory
  , UsageInBytes(..)
  , LimitInBytes(..)
  , Failcnt(..)
  , MaxUsageInBytes(..)
  , SoftLimitInBytes(..)
  -- ** memory+Swap
  , MemswUsageInBytes(..)
  , MemswLimitInBytes(..)
  , MemswFailcnt(..)
  , MemswMaxUsageInBytes(..)
  , MemswSoftLimitInBytes(..)
  -- ** TCP
  , TCPLimitInBytes(..)
  , TCPUsageInBytes(..)
  , TCPFailcnt(..)
  , TCPMaxUsageInBytes(..)
  )
  where

import BasicPrelude
import Data.Default
import System.Linux.Cgroups.Types

-- basic CgroupValue instance
#define _cgroupV(a,b) \
instance CgroupValue a where \
  subsystem _ = subMemory; \
  param _ = b

#define _cgroupR1(a) \
instance CgroupRead a where \
  unprint = a . read

#define _cgroupRB(a) \
instance CgroupRead a where \
  unprint "1" = a True; \
  unprint _   = a False

#define _cgroupW1(a) \
instance CgroupBox a where \
  pprint (a v) = show v

#define _cgroupWB(a) \
instance CgroupBox a where \
  pprint (a True) = "1"; \
  pprint (a False)= "0"
subMemory = Controller "memory"

-- | show current res_counter usage for memory
newtype UsageInBytes = UsageInBytes { unUsageInBytes :: Int }
_cgroupV(UsageInBytes,"usage_in_bytes")
_cgroupR1(UsageInBytes)

-- | show current res_counter usage for memory+Swap
newtype MemswUsageInBytes = MemswUsageInBytes { unMemswUsageInBytes :: Int }
_cgroupV(MemswUsageInBytes,"memsw.usage_in_bytes")
_cgroupR1(MemswUsageInBytes)

-- | set/show limit of memory usage
newtype LimitInBytes = LimitInBytes { unLimitInBytes :: Int }
_cgroupV(LimitInBytes, "limit_in_bytes")
_cgroupR1(LimitInBytes)
_cgroupW1(LimitInBytes)

-- | set/show limit of memory+SWAP usage
newtype MemswLimitInBytes = MemswLimitInBytes { unMemswLimitInBytes :: Int }
_cgroupV(MemswLimitInBytes, "memsw.limit_in_bytes")
_cgroupR1(MemswLimitInBytes)
_cgroupW1(MemswLimitInBytes)

-- | show the number of memory usage hits limits
newtype Failcnt = Failcnt { unFailcnt :: Int }
_cgroupV(Failcnt, "failcnt")
_cgroupR1(Failcnt)

-- | show the number of memory+Swap hits limits
newtype MemswFailcnt = MemswFailcnt { unMemswFailcnt :: Int }
_cgroupV(MemswFailcnt, "memsw.failcnt")
_cgroupR1(MemswFailcnt)

-- | show max memory usage recorded
newtype MaxUsageInBytes = MaxUsageInBytes { unMaxUsageInBytes :: Int }
_cgroupV(MaxUsageInBytes, "max_usage_in_bytes")
_cgroupR1(MaxUsageInBytes)

-- | show max memory+Swap usage recorded
newtype MemswMaxUsageInBytes = MemswMaxUsageInBytes { unMemswMaxUsageInBytes :: Int }
_cgroupV(MemswMaxUsageInBytes, "memsw.max_usage_in_bytes")
_cgroupR1(MemswMaxUsageInBytes)

newtype SoftLimitInBytes = SoftLimitInBytes { unSoftLimitInBytes :: Int }
_cgroupV(SoftLimitInBytes, "soft_limit_in_bytes")
_cgroupR1(SoftLimitInBytes)
_cgroupW1(SoftLimitInBytes)

newtype MemswSoftLimitInBytes = MemswSoftLimitInBytes { unMemswSoftLimitInBytes :: Int }
_cgroupV(MemswSoftLimitInBytes, "soft_limit_in_bytes")
_cgroupR1(MemswSoftLimitInBytes)
_cgroupW1(MemswSoftLimitInBytes)

data Stat = Stat 
    { statLocal :: !MemStat
    , statTotal :: !MemStat
    , statHierarchicalMemoryLimit :: !Int -- ^ # of bytes of memory limit with regard to hierarchy
                                          --   under which the memory cgroup is
    , statHierarchicalMemswLimit :: !Int  -- ^ # of bytes of memory+swap limit with regard to
                                          --   hierarchy under which memory cgroup is.
    }
data MemStat = MemStat
    { statCache        :: !Int  -- ^ # of bytes of page cache memory.
    , statRss          :: !Int  -- ^ # of bytes of anonymous and swap cache memory.
    , statMappedFile   :: !Int  -- ^ # of bytes of mapped file (includes tmpfs/shmem)
    , statPgpgin       :: !Int  -- ^ # of charging events to the memory cgroup. The charging
                                --     event happens each time a page is accounted as either mapped
                                --     anon page(RSS) or cache page(Page Cache) to the cgroup.
    , statPgpgout      :: !Int  -- ^ # of uncharging events to the memory cgroup. The uncharging
                                --     event happens each time a page is unaccounted from the cgroup.
    , statSwap         :: !Int  -- ^  # of bytes of swap usage
    , statInactiveAnon :: !Int  -- ^  # of bytes of anonymous memory and swap cache memory on
                                --      LRU list.
    , statActiveAnon   :: !Int  -- ^  # of bytes of anonymous and swap cache memory on active
                                --      inactive LRU list.
    , statInactiveFile :: !Int  -- ^  # of bytes of file-backed memory on inactive LRU list.
    , statActiveFile   :: !Int  -- ^  # of bytes of file-backed memory on active LRU list.
    , statUnevictable  :: !Int  -- ^  # of bytes of memory that cannot be reclaimed (mlocked etc).
    }
instance Default MemStat where def = MemStat 0 0 0 0 0 0 0 0 0 0 0
instance Default Stat where def = Stat def def 0 0

statSpec = [("cache", \x (t:_) -> x{statLocal = (statLocal x){statCache = read t}})
           ,("rss",   \x (t:_) -> x{statLocal = (statLocal x){statRss   = read t}})
           ,("mapped_file", \x (t:_) -> x{statLocal = (statLocal x){statMappedFile = read t}})
           ,("pgpgin", \x (t:_) -> x{statLocal = (statLocal x){statPgpgin = read t}})
           ,("pgpgout", \x (t:_) -> x{statLocal = (statLocal x){statPgpgout = read t}})
           ,("swap", \x (t:_) -> x{statLocal = (statLocal x){statSwap = read t}})
           ,("inactive_anon",\x (t:_) -> x{statLocal = (statLocal x){statInactiveAnon = read t}})
           ,("active_anon",\x (t:_) -> x{statLocal = (statLocal x){statActiveAnon = read t}})
           ,("inactive_file",\x (t:_) -> x{statLocal = (statLocal x){statInactiveFile = read t}})
           ,("active_file",\x (t:_) -> x{statLocal = (statLocal x){statActiveFile = read t}})
           ,("unevictable",\x (t:_) -> x{statLocal = (statLocal x){statUnevictable = read t}})
           ,("hierarchical_memory_limit",\x (t:_) -> x{statHierarchicalMemoryLimit = read t})
           ,("hierarchical_memsw_limit",\x (t:_) -> x{statHierarchicalMemswLimit = read t})
           ,("total_cache", \x (t:_) -> x{statTotal = (statTotal x){statCache = read t}})
           ,("total_rss",   \x (t:_) -> x{statTotal = (statTotal x){statRss   = read t}})
           ,("total_mapped_file", \x (t:_) -> x{statTotal = (statTotal x){statMappedFile = read t}})
           ,("total_pgpgin", \x (t:_) -> x{statTotal = (statTotal x){statPgpgin = read t}})
           ,("total_pgpgout", \x (t:_) -> x{statTotal = (statTotal x){statPgpgout = read t}})
           ,("total_swap", \x (t:_) -> x{statTotal = (statTotal x){statSwap = read t}})
           ,("total_inactive_anon",\x (t:_) -> x{statTotal = (statTotal x){statInactiveAnon = read t}})
           ,("total_active_anon",\x (t:_) -> x{statTotal = (statTotal x){statActiveAnon = read t}})
           ,("total_inactive_file",\x (t:_) -> x{statTotal = (statTotal x){statInactiveFile = read t}})
           ,("total_active_file",\x (t:_) -> x{statTotal = (statTotal x){statActiveFile = read t}})
           ,("total_unevictable",\x (t:_) -> x{statTotal = (statTotal x){statUnevictable = read t}})
           ]

_cgroupV(Stat, "stat")
instance CgroupRead Stat where unprint = fromLines statSpec

-- |  set/show hierarchical account enabled
newtype UseHierarchy = UseHierarchy { unUseHierarchy :: Bool }
_cgroupV(UseHierarchy, "use_hierarchy")
_cgroupRB(UseHierarchy)
_cgroupWB(UseHierarchy)

-- |  trigger forced move charge to parent
newtype ForceEmpty = ForceEmpty { unForceEmpty :: Bool }
_cgroupV(ForceEmpty, "force_empty")
_cgroupRB(ForceEmpty)
_cgroupWB(ForceEmpty)

-- | set/show swappiness parameter of vmscan
--   (See sysctl's vm.swappiness)
newtype Swappines = Swappines { unSwappines :: Int }
_cgroupV(Swappines, "swappines")
_cgroupR1(Swappines)
_cgroupW1(Swappines)

-- | set/show controls of moving charges
newtype MoveChargeAtMigrate = MoveChargeAtMigrate { unMoveChargeAtMigrate :: Bool }
_cgroupV(MoveChargeAtMigrate, "move_charge_at_migrate")
_cgroupRB(MoveChargeAtMigrate)
_cgroupWB(MoveChargeAtMigrate)

-- | set/show oom controls.
newtype OOMControl = OOMControl { unOOMControl :: Int }
_cgroupV(OOMControl, "oom_control")
_cgroupR1(OOMControl)
_cgroupW1(OOMControl)


newtype TCPLimitInBytes = TCPLimitInBytes { unTCPLimitInBytes :: Int}
_cgroupV(TCPLimitInBytes, "tcp.limit_in_bytes")
_cgroupR1(TCPLimitInBytes)
_cgroupW1(TCPLimitInBytes)

newtype TCPUsageInBytes = TCPUsageInBytes { unTCPUsageInBytes :: Int}
_cgroupV(TCPUsageInBytes, "tcp.usage_in_bytes")
_cgroupR1(TCPUsageInBytes)

newtype TCPFailcnt = TCPFailcnt { unTCPFailcnt :: Int }
_cgroupV(TCPFailcnt, "tcp.failcnt")
_cgroupR1(TCPFailcnt)

newtype TCPMaxUsageInBytes = TCPMaxUsageInBytes { unTCPMaxUsageInBytes :: Int}
_cgroupV(TCPMaxUsageInBytes, "tcp.max_usage_in_bytes")
_cgroupR1(TCPMaxUsageInBytes)
