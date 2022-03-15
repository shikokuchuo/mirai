library(mirai)

m <- eval_mirai({
  q <- m + n + 1L
  q / m
}, m = 2L, n = 3L)
invisible(inherits(call_mirai(m), "mirai") || stop())
invisible(m$value == 3L || stop())
invisible(identical(call_mirai(m), m) || stop())

