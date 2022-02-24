library(mirai)

mirai <- eval_mirai({
  q <- m + n + 1L
  q / m
}, m = 2L, n = 3L)
invisible(!is.null(mirai$aio) || stop())
invisible(!is.null(mirai$socket) || stop())
invisible(!is.null(call_mirai(mirai)) || stop())
invisible(mirai$value == 3L || stop())
invisible(is.null(mirai$aio) || stop())
invisible(is.null(mirai$socket) || stop())
invisible(identical(call_mirai(mirai), mirai) || stop())

