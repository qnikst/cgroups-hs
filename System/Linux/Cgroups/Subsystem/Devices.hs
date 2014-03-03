{-# LANGUAGE OverloadedStrings #-}
module System.Linux.Cgroups.Subsystem.Devices
  ( subDevices
    -- * datatypes
  , Device(..)
  , DevNumber(..)
  , DevAccess(..)
  , DevType(..)
    -- * functions
  , addDeviceRule
  , removeDeviceRule
  )
  where

-- TODO: read Device

import BasicPrelude
import Data.Text (Text)
import qualified Data.Text as T
import Filesystem
import System.Linux.Cgroups.Types
import System.Linux.Cgroups.Device

subDevices = Controller "devices"


-- | cgroup rule
data Device = Device
      { daType   :: !DevType       -- ^ 'DeviceType' 
      , daDev    :: !DeviceN       -- ^ major minor number of device
      , daAccess :: ![DevAccess]   -- ^ 'DeviceAccess'
      } deriving (Show, Eq)


-- | access to device node
data DevAccess = AccessRead         -- ^ read
               | AccessWrite        -- ^ write
               | AccessNod          -- ^ mknod
               deriving (Eq, Show)

instance TextSerial DevAccess where
  tencode AccessRead = "r"
  tencode AccessWrite = "w"
  tencode AccessNod   = "m"
  tdecode t | t == "r" = AccessRead
            | t == "w" = AccessWrite
            | t == "m" = AccessNod
            | otherwise = error . T.unpack $ "non parsable access type "++t

-- | device type
data DevType = DeviceAll         -- ^ any
             | DeviceChar        -- ^ character device
             | DeviceBlock       -- ^ block device
             deriving (Eq, Show)

instance TextSerial DevType where
  tencode DeviceAll   = "a"
  tencode DeviceChar  = "c"
  tencode DeviceBlock = "b"
  tdecode t | t == "a" = DeviceAll
            | t == "c" = DeviceChar
            | t == "b" = DeviceBlock

instance CgroupValue Device where
  subsystem _ = subDevices
  param     _ = "list"

instance TextSerial Device where
  tdecode t = let [t,ms,as] = words t
              in Device (tdecode t) 
                        (tdecode ms) 
                        (T.foldl' (\l t -> tdecode (T.singleton t):l) [] as)
  tencode (Device t dn ls) = T.intercalate " " $ [tencode t, tencode dn] ++ map tencode ls

instance CgroupRead Device where
  unprint = tdecode

addDeviceRule :: Hierarchy Absolute -> Device -> IO ()
addDeviceRule (Hierarchy _ (Cgroup f)) v = writeTextFile (f </> "devices.allow") (tencode v)

removeDeviceRule :: Hierarchy Absolute -> Device -> IO ()
removeDeviceRule (Hierarchy _ (Cgroup f)) v = writeTextFile (f </> "devices.deny") (tencode v)
