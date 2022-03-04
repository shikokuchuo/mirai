library(mirai)

m <- eval_mirai({
  q <- m + n + 1L
  q / m
}, m = 2L, n = 3L)
invisible(!is.null(m[["aio"]]) || stop())
invisible(!is.null(m[["con"]]) || stop())
invisible(!is.null(.subset2(m, "keep.raw")) || stop())
invisible(inherits(call_mirai(m), "mirai") || stop())
invisible(m$data == 3L || stop())
invisible(is.null(.subset2(m, "con")) || stop())
invisible(identical(call_mirai(m), m) || stop())

