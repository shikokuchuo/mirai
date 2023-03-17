# Copyright (C) 2022-2023 Hibiki AI Limited <info@hibiki-ai.com>
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
#' Lightweight parallel code execution, local or distributed across the network.
#'     Designed for simplicity, a 'mirai' evaluates an arbitrary expression
#'     asynchronously, resolving automatically upon completion. Built on
#'     'nanonext' and 'NNG' (Nanomsg Next Gen), uses scalability protocols not
#'     subject to R connection limits and transports faster than TCP/IP where
#'     applicable.
#'
#' @section Notes:
#'
#'     For local mirai processes, the default transport for inter-process
#'     communications is platform-dependent: abstract Unix domain sockets on
#'     Linux, Unix domain sockets on MacOS, Solaris and other POSIX platforms,
#'     and named pipes on Windows.
#'
#'     This may be overriden if required by specifying 'url' in the
#'     \code{\link{daemons}} interface, and starting server and/or dispatcher
#'     processes manually using \code{\link{server}} and \code{\link{dispatcher}}
#'     respectively, on the same machine.
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
#' @importFrom nanonext base64dec call_aio context is_error_value mclock msleep
#'     opt parse_url random recv recv_aio request send socket stat stop_aio
#'     unresolved
#'
#' @docType package
#' @name mirai-package
#'
NULL

.onLoad <- function(libname, pkgname) {
  .command <<- switch(.Platform[["OS.type"]],
                      windows = file.path(R.home("bin"), "Rscript.exe"),
                      file.path(R.home("bin"), "Rscript"))
  .urlfmt <<- switch(Sys.info()[["sysname"]],
                     Linux = "abstract://n%.f",
                     Windows = "ipc://n%.f",
                     "ipc:///tmp/n%.f")
}

.onUnload <- function(libpath) for (i in names(..)) daemons(0L, .compute = i)

.command <- NULL
.urlfmt <- NULL
.statnames <- c("status_online", "status_busy", "tasks_assigned", "tasks_complete", "instance #")
.. <- `[[<-`(new.env(hash = FALSE), "default", new.env(hash = FALSE))
.__scm__. <- base64dec("WAoAAAADAAQCAQADBQAAAAAFVVRGLTgAAAD8", convert = FALSE)

