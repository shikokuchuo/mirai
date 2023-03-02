library(mirai)
nanotest <- function(x) invisible(x || stop())
nanotestw <- function(x) invisible(suppressWarnings(x) || stop())
nanotestn <- function(x) invisible(is.null(x) || stop())
nanotesterr <- function(x, e = "")
  invisible(grepl(e, tryCatch(x, error = identity)[["message"]], fixed = TRUE) || stop())

nanotest(is.list(daemons()))
nanotest(daemons()[["connections"]] == 0L)
nanotest(daemons()[["daemons"]] == 0L)
nanotest(is.na(daemons()[["nodes"]]))
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
nanotest(length(b) == 3L || length(b$data) == 3L)
nanotest(is.character(b) || is.character(b$data))
nanotest(daemons(1L) == 1L)
nanotest(daemons(1L, .compute = "new") == 1L)
me <- mirai(mirai())
nanotest(is_mirai_error(call_mirai(me)$data))
nanotest(!is_mirai_interrupt(me$data))
nanotest(is_error_value(me[["data"]]))
df <- data.frame(a = 1, b = 2)
dm <- mirai(as.matrix(df), .args = list(df), .timeout = 1000L)
nanotest(is_mirai(call_mirai(dm)))
nanotest(!unresolved(dm))
nanotest(is.matrix(dm$data))
nanotestn(stop_mirai(dm))
nanotest(daemons()[["daemons"]] == 1L)
nanotest(daemons(0L) == 0L)
nanotest(daemons()[["daemons"]] == 0L)
nanotest(daemons(value <- sprintf(mirai:::.urlfmt, runif(1, 1000000, 9999999))) == value)
nanotest(daemons(value <- sprintf(mirai:::.urlfmt, runif(1, 1000000, 9999999))) == value)
nanotestw(daemons(0L) == 0L)
mn <- mirai("test", .compute = "new")
nanotest(call_mirai(mn)$data == "test")
nanotest(daemons(0L, .compute = "new") == 0L)
if (Sys.getenv("NOT_CRAN") == "set") {
  nanotest(daemons(1, nodes = 1, walltime = 100000L) == 1L)
  mq <- mirai("queue")
  nanotest(call_mirai(mq)$data == "queue")
  nanotest(daemons()[["nodes"]] == 1L)
  nanotest(daemons(0) == 0L)
  nanotest(daemons("tcp://:5555", nodes = 2) == 1L)
  nanotest((nodes <- sum(daemons()[["nodes"]])) == 0 || nodes == 5L)
  nanotest(daemons(0) == 0L)
}
nanotesterr(daemons("URL"), "argument")
nanotesterr(daemons(-1), "zero")
nanotesterr(daemons(raw(0L)), "numeric, character")
nanotesterr(server("URL"), "argument")
nanotest(daemons(0L) == 0L)
Sys.sleep(2.1)
me$data
m
b
nanotest(is_mirai_interrupt(r <- mirai:::mk_interrupt_error()))
r

