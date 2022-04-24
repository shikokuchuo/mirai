library(mirai)
nanotest <- function(x) invisible(x || stop())

m <- eval_mirai({
  q <- m + n + 1L
  q / m
}, m = 2L, n = 3L)
nanotest(inherits(call_mirai(m), "mirai"))
nanotest(m$data == 3L)
nanotest(identical(call_mirai(m), m))
nanotest(is_mirai(m) == TRUE)
nanotest(daemons(1L) == 1L)
dm <- eval_mirai(as.matrix(df), df = data.frame())
nanotest(inherits(call_mirai(dm), "mirai"))
nanotest(unresolved(dm) == FALSE)
nanotest(daemons(0L) == -1L)
