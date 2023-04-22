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
#' Lightweight parallel code execution and distributed computing. Designed for
#'     simplicity, a 'mirai' evaluates an R expression asynchronously, on local
#'     or network resources, resolving automatically upon completion. Features
#'     efficient task scheduling, scalability beyond R connection limits, and
#'     transports faster than TCP/IP for inter-process communications, courtesy
#'     of 'nanonext' and 'NNG' (Nanomsg Next Gen).
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
#'     respectively, on the local machine.
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
#' @importFrom nanonext call_aio context cv cv_reset cv_value dial
#'     is_error_value listen lock mclock msleep opt opt<- parse_url pipe_notify
#'     random recv recv_aio_signal request request_signal send sha1 socket stat
#'     stop_aio unresolved until wait
#'
#' @docType package
#' @name mirai-package
#'
NULL

.onLoad <- function(libname, pkgname)
  switch(Sys.info()[["sysname"]],
         Linux = {
           .command <<- file.path(R.home("bin"), "Rscript")
           .urlfmt <<- "abstract://%s"
           .intmax <<- .Machine[["integer.max"]]
         },
         Windows = {
           .command <<- file.path(R.home("bin"), "Rscript.exe")
           .urlfmt <<- "ipc://%s"
           .intmax <<- .Machine[["integer.max"]]
         },
         {
           .command <<- file.path(R.home("bin"), "Rscript")
           .urlfmt <<- "ipc:///tmp/%s"
           .intmax <<- .Machine[["integer.max"]]
         })

.command <- NULL
.urlfmt <- NULL
.intmax <- NULL

.. <- `[[<-`(new.env(hash = FALSE), "default", new.env(hash = FALSE))

