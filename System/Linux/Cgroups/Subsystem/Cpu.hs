{-# LANGUAGE OverloadedStrings #-}
module System.Linux.Cgroups.Subsystem.Cpu 
  ( subCpu
  , Share(..)
  ) where

import BasicPrelude
import Data.Default
import System.Linux.Cgroups.Types

subCpu = Controller "cpu"

-- | Cpu share describle percentage of processor time can be
-- taken by process
newtype Share = Share { unShare :: Int} deriving (Read, Show, Eq)

instance CgroupValue Share where
  subsystem _ = subCpu
  param _ = "shares"

instance CgroupRead Share where
  unprint = Share . read 

instance CgroupBox Share where
  pprint = show . unShare

instance Default Share where
  def = Share 1024
 



