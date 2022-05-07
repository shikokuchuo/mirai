#' mirai: Minimalist Async Evaluation Framework for R
#'
#' Extremely simple and lightweight method for concurrent / parallel code
#'     execution, built on 'nanonext' and 'NNG' (Nanomsg Next Gen) technology.
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
#' @importFrom nanonext call_aio context is_nul_byte recv request send send_aio
#'     socket stop_aio unresolved .mirai_scm
#' @importFrom stats runif
#'
#' @docType package
#' @name mirai-package
#'
NULL

.onLoad <- function(libname, pkgname) {
  daemons <- daemons()
  daemons <<- daemons
  invisible()
}

.onUnload <- function(libpath) {
  daemons(0L)
  invisible()
}

#' @export
nanonext::is_nul_byte

#' @export
nanonext::unresolved

