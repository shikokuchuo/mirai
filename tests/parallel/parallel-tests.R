rversion <- .subset2(getRversion(), 1L)
(rversion[1L] >= 4 && rversion[2L] >= 4 || rversion[1L] >= 5) ||
  stop("Requirement R >= 4.4 not met, stopped\n")

library(mirai)
library(parallel)

nanotest <- function(x) invisible(x || stop("is not TRUE when expected to be TRUE"))
nanotestn <- function(x) invisible(is.null(x) || stop("is not NULL when expected to be NULL"))
nanotesti <- function(a, b) invisible(identical(a, b) || stop("the arguments are not identical as expected"))
nanotesterr <- function(x, e = "")
  invisible(grepl(e, tryCatch(x, error = identity)[["message"]], fixed = TRUE) || stop("expected error message '", e, "' not generated"))

# benchmarking
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

cat("\n2. Starting tests\n")
start <- nanonext::mclock()

# testing functions from parallel package
cluster <- make_cluster(2)
nanotest(inherits(cluster, "miraiCluster"))
nanotest(inherits(cluster, "cluster"))
nanotest(length(cluster) == 2L)

clusterSetRNGStream(cluster, 123)
j <- clusterEvalQ(cluster, expr = .GlobalEnv[[".Random.seed"]])
a <- parSapply(cluster, 1:4, rnorm)

setDefaultCluster(cluster)
res <- parLapply(X = 1:10, fun = rnorm)
nanotest(is.list(res) && length(res) == 10L)
nanotest(is.double(res[[1L]]) && length(res[[1L]]) == 1L)
nanotest(is.double(res[[10L]]) && length(res[[10L]]) == 10L)
res <- parLapplyLB(X = 1:10, fun = rnorm)
nanotest(is.list(res) && length(res) == 10L)
nanotest(is.double(res[[1L]]) && length(res[[1L]]) == 1L)
nanotest(is.double(res[[10L]]) && length(res[[10L]]) == 10L)
nanotesti(parSapply(NULL, 1:4, factorial), c(1, 2, 6, 24))
nanotesti(parSapplyLB(NULL, 1:8, factorial), c(1, 2, 6, 24, 120, 720, 5040, 40320))
df <- data.frame(a = c(1, 2, 3), b = c(6, 7, 8))
nanotesti(parApply(cluster, df, 2, sum), `names<-`(c(6, 21), c("a", "b")))
nanotesti(parCapply(cluster, df, sum), `names<-`(c(6, 21), c("a", "b")))
nanotesti(parRapply(cluster, df, sum), `names<-`(c(7, 9, 11), c("1", "2", "3")))
res <- clusterEvalQ(expr = .GlobalEnv[[".Random.seed"]][[1L]])
nanotesti(res[[1L]], res[[2L]])
nanotesterr(clusterEvalQ(cluster, elephant()), "Error in elephant(): could not find function \"elephant\"")

# testing examples from parallel package
nanotest(inherits(cl <- make_cluster(2), "miraiCluster"))
nanotest(attr(cl, "id") != attr(cluster, "id"))

clusterSetRNGStream(cl, 123)
k <- clusterEvalQ(cl, expr = .GlobalEnv[[".Random.seed"]])
b <- parSapply(cl, 1:4, rnorm)
nanotesti(j, k)
nanotesti(a, b)

identical(clusterApply(cl, 1:2, get("+"), 3), list(4, 5))
xx <- 1
clusterExport(cl, "xx", environment())
nanotesti(clusterCall(cl, function(y) xx + y, 2), list(3,3))
nanotesti(clusterMap(cl, function(x, y) seq_len(x) + y, c(a =  1, b = 2, c = 3), c(A = 10, B = 0, C = -10)),
          list (a = 11, b = c(1, 2), c = c(-9, -8, -7)))
nanotesti(parSapply(cl, 1:20, get("+"), 3), as.double(4:23))

nanotestn(stopCluster(cl))
nanotestn(stop_cluster(cluster))
nanotesterr(parLapply(cluster, 1:10, runif), "cluster is no longer active")
cat(sprintf(" + all tests successful in %.3f s\n\n", (nanonext::mclock() - start) / 1000))
