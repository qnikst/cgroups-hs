{-# LANGUAGE OverloadedStrings #-}
module Main 
  (main
  , go
  ) where

import BasicPrelude
import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map as M
import Filesystem
import Filesystem.Path.CurrentOS
import System.INotify
import System.Linux.Cgroups

main = getArgs >>= go

go (path:_) = do
  let upath = unsafeCgroup $ fromText path
      tasks = tasksFile upath
  m <- newEmptyMVar
  withINotify $ \notify ->  
    bracket (addWatch notify [AllEvents] (encodeString tasks) 
                (\e -> do t <- getTasks' upath
                          print t
                          when (length t < 50) (putMVar m ())))
            (removeWatch)
            (const $ takeMVar m)
