
--  1) mount -t tmpfs cgroup_root /sys/fs/cgroup
--  2) mkdir /sys/fs/cgroup/cpuset
--  3) mount -t cgroup -ocpuset cpuset /sys/fs/cgroup/cpuset
--  4) Create the new cgroup by doing mkdir's and write's (or echo's) in
--      the /sys/fs/cgroup virtual file system.
--  5) Start a task that will be the "founding father" of the new job.
--  6) Attach that task to the new cgroup by writing its PID to the
--      /sys/fs/cgroup/cpuset/tasks file for that cgroup.
--  7) fork, exec or clone the job tasks from this founding father task.
--

main = do
  -- root <- runPriv $ mountCgroups "/sys/fs/cgroup
  -- mkSubsystem CpuSet root "cpuset"
  cpuset <- lookupCgroupRoot cpuSet
  charlie <- newCgroup cpuset "Charlie"  
  moveSelf charlie
  exec ...
