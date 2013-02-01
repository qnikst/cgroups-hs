module System.Linux.Cgroups.Subsystem.Memory
  where

-- | show current res_counter usage for memory
newtype UsageInBytes = UsageInBytes { unUsageInBytes :: Int }

instance CgroupValue UsageInBytes where
  subsystem _ = "cpu"
  param _ = "shares"

instance CgroupRead UsageInBytes where unprint = UsageInBytes . read

newtype MemswUsageInBytes = MemswUsageInBytes { unMemswUsageInBytes :: Int }

newtype LimitInBytes = LimitInBytes { unLimitInBytes :: Int }

newtype Failcnt = Failcnt { unFailcnt :: Int }

newtype MemswFailcnt = MemswFailCnt { unMemswFailcnt :: Int }

newtype MaxUsageInBytes = MaxUsageInBytes { unMaxUsageInBytes :: Int }

newtype MemswMaxUsageInBytes = MemswMaxUsageInBytes { unMemswMaxUsageInBytes :: Int }

newtype SoftLimitInBytes = SoftLimitInBytes { unSoftLimitInBytes :: Int }

data Stat

newtype UseHierarchy = UseHierarchy { unUseHierarchy :: Bool }

newtype ForceEmpty = UseForceEmpty { unForceEmpty :: Bool }

newtype Swappines = Swappines { unSwappines :: Int }

newtype MoveChargeAtMigrate = MoveChargeAtMigrate { unMoveChargeAtMigrate :: Bool }

newtype OOMControl = OOMControl { unOOMControl :: Int }

data NumaStat

newtype TCPLimitInBytes = TCPLimitInBytes { unTCPLimitInBytes :: Int}

newtype TCPUsageInBytes = TCPUsageInBytes { unUsageInBytes :: Int}

newtype TCPFailcnt = TCPFailcnt { unTCPFailcnt :: Int }

newtype TCPMaxUsageInBytes = TCPMaxUsageInBytes { unMaxUsageInBytes :: Int}
