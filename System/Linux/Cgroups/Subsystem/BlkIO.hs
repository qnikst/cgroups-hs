{-# LANGUAGE OverloadedStrings, CPP #-}
module System.Linux.Cgroups.Subsystem.BlkIO
  where

import System.Linux.Cgroups.Types
import System.Linux.Cgroups.Device

subBlkIO = Controller "blkio"

-- TODO: remove from blk.weight

#define _cgroupV(a,b) \
instance CgroupValue a where \
  subsystem _ = subBlkIO; \
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

-- | Specifies per cgroup weight. This is default weight of the group
--   on all the devices until and unless overridden by per device rule.
--   (See blkio.weight_device).
--   Currently allowed range of weights is from 10 to 1000.
newtype Weight = Weight { unWeight :: Int }
instance Default Weight where def = 1000
_cgroupV(Weight)
_cgroupR1(Weight)
_cgroupW1(Weight)

-- | One can specify per cgroup per device rules using this interface.
--   These rules override the default value of group weight as specified
--   by blkio.weight.
--
--   Following is the format.
--
--   # echo dev_maj:dev_minor weight > blkio.weight_device
--   Configure weight=300 on /dev/sdb (8:16) in this cgroup
--   # echo 8:16 300 > blkio.weight_device
--   # cat blkio.weight_device
--   dev     weight
--   8:16    300
--
--   Configure weight=500 on /dev/sda (8:0) in this cgroup
--   # echo 8:0 500 > blkio.weight_device
--   # cat blkio.weight_device
--   dev     weight
--   8:0     500
--   8:16    300
--
--   Remove specific weight for /dev/sda in this cgroup
--   # echo 8:0 0 > blkio.weight_device
--   # cat blkio.weight_device
--   dev     weight
--   8:16    300

data WeightDevice = WeightDevice [(Device, Int)]
_cgroupV(WeightDevice, "weight_device")
instance CgroupRead WeightDevice where
  unprint t = 
    let (_:ts) = lines t
    in WeightDevice $! map (\(d,w) -> (read d,read w)) $ map words ts

-- | - blkio.time
--     disk time allocated to cgroup per device in milliseconds. First
--     two fields specify the major and minor number of the device and
--     third field specifies the disk time allocated to group in
--     milliseconds.
data BlkTime = BlkTime [(Device, Int)]


