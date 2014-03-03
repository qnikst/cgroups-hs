module System.Linux.Cgroups.Device
  ( DeviceN(..)
  , DevNumber
  ) where

import BasicPrelude
import qualified Data.Text as T
import System.Linux.Cgroups.Types

data DeviceN = DeviceN { deMajor :: DevNumber
                       , deMinor :: DevNumber
                       }
                       deriving (Eq, Show)

instance TextSerial DeviceN where
  tencode (DeviceN ma mi) = T.concat [tencode ma,":",tencode mi]
  tdecode t = let (tma, tmi) = T.span (==':') t
              in DeviceN (tdecode tma) (tdecode tmi)

-- | number of a device node
data DevNumber = All               -- ^ any number
               | One Int           -- ^ exact number
               deriving (Eq, Show)

instance TextSerial DevNumber where
  tencode All = "*"
  tencode (One i) = show i
  tdecode t | t == "*" = All
            | otherwise = One (read t)

