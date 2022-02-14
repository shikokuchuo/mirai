library(mirai)

mirai <- eval_mirai(m + n + 1L, m = 2L, n = 3L)
invisible(!is.null(mirai$aio) || stop())
invisible(!is.null(mirai$socket) || stop())
call_mirai(mirai)
invisible(mirai$value == 6L || stop())
invisible(is.null(mirai$aio) || stop())
invisible(is.null(mirai$socket) || stop())
invisible(identical(call_mirai(mirai), mirai) || stop())

