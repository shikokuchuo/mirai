library(mirai)
nanotest <- function(x) invisible(x || stop())
nanotesterr <- function(x) invisible(tryCatch(x, error = function(e) TRUE) || stop())

nanotest(daemons("view") == 0L)
n <- 3L
m <- mirai({
  Sys.sleep(0.1)
  q <- m + n + 1L
  q / m
}, m = 2L, .args = list(n))
b <- m$data %>>% rnorm %>>% as.character()
b
nanotest(inherits(call_mirai(m), "mirai"))
nanotest(m$data == 3L)
nanotest(identical(call_mirai(m), m))
nanotest(is_mirai(m))
nanotest(length(b$data) == 3L)
nanotest(is.character(b$data))
nanotest(daemons(c(1L, 2L)) == 1L)
me <- mirai(mirai())
nanotest(is_mirai_error(call_mirai(me)$data))
nanotest(is_error_value(me[["data"]]))
df <- data.frame(a = 1, b = 2)
dm <- eval_mirai(as.matrix(df), .args = list(df), .timeout = 1000L)
nanotest(is_mirai(call_mirai(dm)))
nanotest(!unresolved(dm))
nanotest(is.matrix(dm$data))
nanotest(is.null(stop_mirai(dm)))
nanotest(daemons("view") == 1L)
nanotest(daemons(0L) == -1L)
nanotest(daemons("view") == 0L)
nanotest(daemons(n = 0L, .url = sprintf(mirai:::.urlfmt, 01010101)) == 1L)
nanotest(daemons(0L) == -1L)
nanotest(is.null(daemons()))
nanotesterr(daemons("test"))
nanotesterr(daemons(.url = 0L))
Sys.sleep(1L)
me$data
m
b

