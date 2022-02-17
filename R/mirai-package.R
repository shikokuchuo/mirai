#' mirai: Minimalist Async Evaluation Framework for R
#'
#' An extremely simple and lightweight method for concurrent / parallel code
#'     execution, built on the nanonext R package and NNG (Nanomsg Next Gen).
#'     mirai is Japanese for 'future'.
#'
#' @section Links:
#'
#'     mirai website: \url{https://shikokuchuo.net/mirai/}\cr
#'     mirai on CRAN: \url{https://cran.r-project.org/package=mirai}
#'
#'     nanonext website: \url{https://shikokuchuo.net/nanonext/}\cr
#'     nanonext on CRAN: \url{https://cran.r-project.org/package=nanonext}
#'
#'     NNG website: \url{https://nng.nanomsg.org/}
#'
#' @encoding UTF-8
#' @author Charlie Gao \email{charlie.gao@@shikokuchuo.net}
#'     (\href{https://orcid.org/0000-0002-0750-061X}{ORCID})
#'
#' @importFrom nanonext call_aio context recv_ctx request send_aio send_ctx
#'     socket stop_aio
#' @importFrom stats runif
#'
#' @docType package
#' @name mirai-package
#'
NULL

