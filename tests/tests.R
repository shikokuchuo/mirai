library(mirai)

mirai <- eval_mirai({
  q <- m + n + 1L
  q / m
}, m = 2L, n = 3L)
invisible(!is.null(mirai[["aio"]]) || stop())
invisible(!is.null(mirai[["con"]]) || stop())
invisible(!is.null(.subset2(mirai, "keep.raw")) || stop())
invisible(inherits(call_mirai(mirai), "mirai") || stop())
invisible(mirai$data == 3L || stop())
invisible(is.null(.subset2(mirai, "con")) || stop())
invisible(identical(call_mirai(mirai), mirai) || stop())

