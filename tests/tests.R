library(mirai)
nanotest <- function(x) invisible(x || stop())

nanotest(daemons("view") == 0L)
if (mirai:::.sysname == "Windows")
  nanotest(daemons(c(1L, 2L)) == 1L)
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
b
if (mirai:::.sysname != "Windows")
  nanotest(daemons(c(1L, 2L)) == 1L)
df <- data.frame(a = 1, b = 2)
dm <- eval_mirai(as.matrix(df), .args = list(df), .timeout = 1000L)
nanotest(is_mirai(call_mirai(dm)))
nanotest(!unresolved(dm))
nanotest(is.matrix(dm$data))
nanotest(is.null(stop_mirai(dm)))
me <- mirai(stop())
nanotest(is_mirai_error(call_mirai(me)$data))
nanotest(is_error_value(me[["data"]]))
me$data
nanotest(daemons("view") == 1L)
nanotest(daemons(0L) == -1L)
nanotest(daemons("view") == 0L)
nanotest(daemons(n = 0L, .url = "tcp://:5555") == 1L)
nanotest(daemons(0L) == -1L)
nanotest(is.null(daemons()))
Sys.sleep(2L)
nanotest(tryCatch(daemons("test"), error = function(e) TRUE))
nanotest(tryCatch(daemons(.url = 0L), error = function(e) TRUE))
m

