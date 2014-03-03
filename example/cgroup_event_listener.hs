import Prelude()
import BasicPrelude
import Filesystem.Path.CurrentOS
import System.Linux.Cgroups
import System.Posix.Eventfd

go path = do
  let upath = unsafeCgroup $ fromText path
  bracket (eventfd [])
          eventfdClose
          (\efd -> do undefined
                      fdh <- registerNotification efd upath value
                      let loop = do eventfdRead efd
                                    doesCgroupExists upath

                                    access cfd
          ) 
