# Copyright (C) 2022-2024 Hibiki AI Limited <info@hibiki-ai.com>
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
#'     or network resources, resolving automatically upon completion. State of
#'     the art networking and concurrency via 'nanonext' and 'NNG' (Nanomsg Next
#'     Gen) offers reliable and efficient scheduling over fast inter-process
#'     communications or TCP/IP secured by TLS.
#'
#' @section Notes:
#'
#'     For local mirai requests, the default transport for inter-process
#'     communications is platform-dependent: abstract Unix domain sockets on
#'     Linux, Unix domain sockets on MacOS, Solaris and other POSIX platforms,
#'     and named pipes on Windows.
#'
#'     This may be overriden, if desired, by specifying 'url' in the
#'     \code{\link{daemons}} interface and launching daemons using
#'     \code{\link{launch_local}}.
#'
#' @section Reference Manual:
#'
#' \code{vignette("mirai", package = "mirai")}
#'
#' @encoding UTF-8
#' @author Charlie Gao \email{charlie.gao@@shikokuchuo.net}
#'     (\href{https://orcid.org/0000-0002-0750-061X}{ORCID})
#'
#' @importFrom nanonext call_aio call_aio_ collect_aio collect_aio_ .context cv
#'     cv_value dial is_error_value listen lock mclock msleep next_config
#'     nng_error opt opt<- parse_url pipe_notify random reap recv
#'     recv_aio_signal request request_signal send set_promise_context socket
#'     stat stop_aio strcat tls_config unresolved until wait write_cert
#' @importFrom stats rexp
#' @importFrom utils .DollarNames
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
    arglen = "'args' and/or 'url' must be of length 1 or the same length",
    cluster_inactive = "cluster is no longer active",
    correct_context = "'host' must be specified if not using directly in a function argument",
    daemons_unset = "a numeric value for 'url' requires daemons to be set",
    dot_required = "'.' must be an element of the character vector(s) supplied to 'args'",
    missing_expression = "missing expression, perhaps wrap in {}?",
    missing_url = "at least one URL must be supplied for 'url' or 'n' must be at least 1",
    named_args = "all '...' arguments must be named, unless supplying an environment",
    n_one = "'n' must be 1 or greater",
    n_zero = "the number of daemons must be zero or greater",
    numeric_n = "'n' must be numeric, did you mean to provide 'url'?",
    register_cluster = "this function requires a more recent version of R",
    requires_local = "SSH tunnelling requires 'url' hostname to be '127.0.0.1' or 'localhost'",
    refhook_invalid = "'refhook' must be a list of 2 functions or NULL",
    single_url = "only one 'url' should be specified",
    sync_timeout = "initial sync with dispatcher timed out after 10s",
    url_spec = "numeric value for 'url' is out of bounds",
    wrong_dots = "'...' arguments should only be of integer, numeric or logical type"
  ),
  hash = TRUE
)

.intmax <- .Machine[["integer.max"]]
.limit_short <- 5000L
.limit_long <- 10000L
