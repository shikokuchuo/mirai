library(mirai)
nanotest <- function(x) invisible(x || stop())

m <- eval_mirai({
  q <- m + n + 1L
  q / m
}, m = 2L, n = 3L)
nanotest(inherits(call_mirai(m), "mirai"))
nanotest(m$data == 3L)
nanotest(identical(call_mirai(m), m))

