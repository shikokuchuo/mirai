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
#' Simple and lightweight parallel code execution, local or distributed across
#'     the network, built on 'nanonext' and 'NNG' (Nanomsg Next Gen) technology.
#'     A 'mirai' (Japanese for 'future') evaluates an arbitrary expression
#'     asynchronously, resolving automatically upon completion.
#'
#' @section Implementation:
#'
#'     'mirai' also inherit from the class 'recvAio' and are asynchronous
#'     message receive objects using the \{nanonext\} framework.
#'
#'     Functions \code{\link{call_mirai}}, \code{\link{stop_mirai}} and
#'     \code{\link{unresolved}} are directly assigned from 'nanonext' for
#'     efficiency. As a result, their argument name remains 'aio' instead of
#'     'mirai'. This should be of no practical significance as they all take a
#'     single argument, hence normal usage precludes use of the argument name.
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
#' @importFrom nanonext call_aio context is_error_value msleep random recv
#'     request send send_aio socket stop_aio unresolved
#'
#' @docType package
#' @name mirai-package
#'
NULL

.onLoad <- function(libname, pkgname) daemons <<- daemons()

.onUnload <- function(libpath) invisible(daemons(0L))

.miraiclass <- c("mirai", "recvAio")
.errorclass <- c("miraiError", "errorValue")
.interrupt <- `class<-`("", c("miraiInterrupt", "errorValue"))
.sysname <- .subset2(Sys.info(), "sysname")
.command <- switch(.sysname,
                   Windows = file.path(R.home("bin"), "Rscript.exe"),
                   file.path(R.home("bin"), "Rscript"))
.urlfmt <- switch(.sysname,
                  Linux = "abstract://n%.f",
                  Windows = "ipc://n%.f",
                  "ipc:///tmp/n%.f")
.__scm__. <- as.raw(c(0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x02,
                      0x01, 0x00, 0x03, 0x05, 0x00, 0x00, 0x00, 0x00, 0x05,
                      0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0xfc))

