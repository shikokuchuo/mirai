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
#'     efficient task scheduling, fast inter-process communications, and
#'     Transport Layer Security over TCP/IP for remote connections, courtesy of
#'     'nanonext' and 'NNG' (Nanomsg Next Gen).
#'
#' @section Notes:
#'
#'     For local mirai requests, the default transport for inter-process
#'     communications is platform-dependent: abstract Unix domain sockets on
#'     Linux, Unix domain sockets on MacOS, Solaris and other POSIX platforms,
#'     and named pipes on Windows.
#'
#'     This may be overriden, if required, by specifying 'url' in the
#'     \code{\link{daemons}} interface and launching daemons using
#'     \code{\link{launch_local}}.
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
#' @importFrom nanonext call_aio .context cv cv_value dial is_error_value listen
#'     lock mclock msleep opt opt<- parse_url pipe_notify random recv
#'     recv_aio_signal request request_signal send send_aio sha1 socket stat
#'     stop_aio strcat tls_config unresolved until wait weakref weakref_value
#'     write_cert
#'
#' @docType package
#' @name mirai-package
#'
NULL

.onLoad <- function(libname, pkgname) {

  .. <<- `[[<-`(new.env(hash = FALSE), "default", new.env(hash = FALSE))
  switch(Sys.info()[["sysname"]],
         Linux = {
           .command <<- file.path(R.home("bin"), "Rscript")
           .urlscheme <<- "abstract://"
         },
         Windows = {
           .command <<- file.path(R.home("bin"), "Rscript.exe")
           .urlscheme <<- "ipc://"
         },
         {
           .command <<- file.path(R.home("bin"), "Rscript")
           .urlscheme <<- "ipc:///tmp/"
         })

}

.. <- NULL
.command <- NULL
.intmax <- .Machine[["integer.max"]]
.urlscheme <- NULL
.timelimit <- 5000L

.messages <- list2env(
  list(
    connection_timeout = "connection to local process timed out after 5s",
    dispatcher_inactive = "a numeric value for 'url' requires daemons to be set",
    dot_required = "'.' must be an element of the character vector supplied to 'args'",
    missing_url = "at least one URL must be supplied for 'url' or 'n' must be at least 1",
    missing_expression = "missing expression, perhaps wrap in {}?",
    n_one = "'n' must be 1 or greater if specified with 'url'",
    n_zero = "the number of daemons must be zero or greater",
    numeric_n = "'n' must be numeric, did you mean to provide 'url'?",
    requires_list = "'.args' must be specified as a list",
    url_spec = "numeric value for 'url' is out of bounds"
  ),
  hash = TRUE
)
