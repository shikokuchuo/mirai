# Copyright (C) 2022 Hibiki AI Limited <info@hibiki-ai.com>
#
# This file is part of mirai.
#
# mirai is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# mirai is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# mirai. If not, see <https://www.gnu.org/licenses/>.

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
#' @importFrom nanonext call_aio context is_nul_byte random recv request send
#'     send_aio socket stop_aio unresolved .mirai_scm
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

