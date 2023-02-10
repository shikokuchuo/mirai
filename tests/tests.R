library(mirai)
nanotest <- function(x) invisible(x || stop())
nanotestw <- function(x) invisible(suppressWarnings(x) || stop())
nanotestn <- function(x) invisible(is.null(x) || stop())
nanotesterr <- function(x, e = "")
  invisible(grepl(e, tryCatch(x, error = identity)[["message"]], fixed = TRUE) || stop())

nanotest(daemons_view() == 0L)
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
nanotest(daemons(1L) == 1L)
me <- mirai(mirai())
nanotest(is_mirai_error(call_mirai(me)$data))
nanotest(!is_mirai_interrupt(me$data))
nanotest(is_error_value(me[["data"]]))
df <- data.frame(a = 1, b = 2)
dm <- eval_mirai(as.matrix(df), .args = list(df), .timeout = 1000L)
nanotest(is_mirai(call_mirai(dm)))
nanotest(!unresolved(dm))
nanotest(is.matrix(dm$data))
nanotestn(stop_mirai(dm))
nanotest(daemons_view() == 1L)
nanotest(daemons(0L) == -1L)
nanotest(daemons_view() == 0L)
nanotest(daemons(n = 0L, .url = sprintf(mirai:::.urlfmt, runif(1, 1000000, 9999999))) == 1L)
nanotestw(daemons(0L) == -1L)
nanotestn(daemons())
if (Sys.getenv("NOT_CRAN") == "set") {
  nanotest(daemons(1, q = TRUE) == 1L)
  mq <- mirai("queue")
  nanotest(call_mirai(mq)$data == "queue")
  nanotest(daemons(0) == -1L)
}
nanotesterr(daemons("test"), "non-numeric")
nanotesterr(daemons(.url = 0L), "non-character")
Sys.sleep(1L)
me$data
m
b
nanotest(is_mirai_interrupt(r <- mirai:::mk_interrupt_error()))
r

