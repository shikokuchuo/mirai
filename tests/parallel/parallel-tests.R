library(mirai)
library(parallel)

# tests moved to main package tests file
# benchmarking:
cluster <- make_cluster(2)
oldcluster <- makePSOCKcluster(2)
cat("\n1. Starting benchmarks\n + with parLapply\n   miraiCluster:\n")
print(system.time(parLapply(cluster, 1:3e6, identity)))
cat("   PSOCKcluster:\n")
print(system.time(parLapply(oldcluster, 1:3e6, identity)))
cat("\n + with parLapplyLB\n   miraiCluster:\n")
print(system.time(parLapplyLB(cluster, 1:3e6, identity)))
cat("   PSOCKcluster:\n")
print(system.time(parLapplyLB(oldcluster, 1:3e6, identity)))
stopCluster(oldcluster)
