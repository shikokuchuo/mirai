# Copyright (C) 2022-2025 Hibiki AI Limited <info@hibiki-ai.com>
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
#' Designed for simplicity, a 'mirai' evaluates an R expression asynchronously
#' in a parallel process, locally or distributed over the network. The result is
#' automatically available upon completion. Modern networking and concurrency,
#' built on 'nanonext' and 'NNG' (Nanomsg Next Gen), ensures reliable and
#' efficient scheduling over fast inter-process communications or TCP/IP secured
#' by TLS. Distributed computing can launch remote resources via SSH or cluster
#' managers. An inherently queued architecture handles many more tasks than
#' available processes, and requires no storage on the file system. Innovative
#' features include support for otherwise non-exportable reference objects,
#' event-driven promises, and asynchronous parallel map.
#'
#' @section Notes:
#'
#'  For local mirai requests, the default transport for inter-process
#'  communications is platform-dependent: abstract Unix domain sockets on Linux,
#'  Unix domain sockets on MacOS, Solaris and other POSIX platforms, and named
#'  pipes on Windows.
#'
#'  This may be overriden, if desired, by specifying 'url' in the
#'  \code{\link{daemons}} interface and launching daemons using
#'  \code{\link{launch_local}}.
#'
#' @section Reference Manual:
#'
#' \code{vignette("mirai", package = "mirai")}
#'
#' @encoding UTF-8
#' @author Charlie Gao \email{charlie.gao@@shikokuchuo.net}
#'   (\href{https://orcid.org/0000-0002-0750-061X}{ORCID})
#'
#' @importFrom nanonext .advance call_aio call_aio_ collect_aio collect_aio_
#'   .context cv cv_signal cv_value dial .interrupt is_error_value .keep listen
#'   lock .mark mclock monitor msleep nng_error opt opt<- parse_url pipe_notify
#'   random read_monitor reap recv recv_aio request send serial_config socket
#'   stat stop_aio tls_config unresolved .unresolved until wait write_cert
#'
"_PACKAGE"

# nocov start
# tested implicitly

.onLoad <- function(libname, pkgname) {

  switch(
    Sys.info()[["sysname"]],
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
    }
  )

}

# nocov end

. <- new.env()
.. <- new.env()
.command <- NULL
.urlscheme <- NULL

._ <- list2env(
  list(
    arglen = "`n` must equal the length of `args`, or either must be 1",
    cluster_inactive = "cluster is no longer active",
    daemons_set = "daemons already set for `%s` compute profile",
    daemons_unset = "daemons must be set to use launchers",
    dispatcher_args = "`dispatcher` must be either TRUE or FALSE",
    dot_required = "`.` must be an element of the character vector(s) supplied to `args`",
    function_required = "`.f` must be of type function, not %s",
    missing_expression = "missing expression, perhaps wrap in {}?",
    missing_url = "`n` must be 1 or greater, or else `url` must be supplied",
    named_args = "all items in `.args` must be named, unless supplying an environment",
    named_dots = "all `...` arguments must be named, unless supplying an environment",
    n_one = "`n` must be 1 or greater",
    n_zero = "the number of daemons must be zero or greater",
    not_found = "compute profile `%s` not found",
    numeric_n = "`n` must be numeric, did you mean to provide `url`?",
    register_cluster = "this function requires a more recent version of R",
    requires_daemons = "daemons must be set prior to a map operation",
    sync_daemons = "initial sync with daemon(s) timed out after 10s",
    sync_dispatcher = "initial sync with dispatcher timed out after 10s"
  ),
  hash = TRUE
)

.intmax <- .Machine[["integer.max"]]
.limit_short <- 5000L
.limit_long <- 10000L
