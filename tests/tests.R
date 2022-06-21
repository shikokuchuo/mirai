library(mirai)
nanotest <- function(x) invisible(x || stop())

nanotest(daemons("view") == 0L)
nanotest(daemons(1L) == 1L)
m <- mirai({
  q <- m + n + 1L
  q / m
}, m = 2L, n = 3L)
b <- m$data %>>% rnorm %>>% as.character()
nanotest(inherits(call_mirai(m), "mirai"))
nanotest(m$data == 3L)
nanotest(identical(call_mirai(m), m))
nanotest(is_mirai(m) == TRUE)
nanotest(length(b$data) == 3L)
nanotest(is.character(b$data))
dm <- eval_mirai(as.matrix(df), df = data.frame(), .timeout = 2000)
nanotest(inherits(call_mirai(dm), "mirai"))
nanotest(unresolved(dm) == FALSE)
nanotest(is.null(stop_mirai(dm)))
nanotest(daemons("view") == 1L)
nanotest(daemons(0L) == -1L)
nanotest(daemons("view") == 0L)
nanotest(is.null(tryCatch(daemons("error"), error = function(e) NULL)))
