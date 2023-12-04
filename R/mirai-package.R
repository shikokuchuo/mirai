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
#'     or network resources, resolving automatically upon completion. Efficient
#'     scheduling over fast inter-process communications or secure TLS
#'     connections over TCP/IP, built on 'nanonext' and 'NNG' (Nanomsg Next
#'     Gen).
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
#' @section Links:
#'
#'     \CRANpkg{mirai} website: \url{https://shikokuchuo.net/mirai/}
#'
#'     \CRANpkg{nanonext} website: \url{https://shikokuchuo.net/nanonext/}
#'
#'     NNG website: \url{https://nng.nanomsg.org/}
#'
#' @encoding UTF-8
#' @author Charlie Gao \email{charlie.gao@@shikokuchuo.net}
#'     (\href{https://orcid.org/0000-0002-0750-061X}{ORCID})
#'
#' @importFrom nanonext base64dec call_aio call_aio_ .context cv cv_value dial
#'     is_error_value listen lock mclock msleep next_config opt opt<- parse_url
#'     pipe_notify random reap recv recv_aio_signal request request_signal send
#'     send_aio socket stat stop_aio strcat tls_config unresolved until wait
#'     write_cert
#' @importFrom parallel nextRNGStream stopCluster
#' @importFrom stats rexp
#'
#' @docType package
#' @name mirai-package
#'
NULL

# nocov start
# tested implicitly

.onLoad <- function(libname, pkgname) {

  . <<- new.env(hash = FALSE, parent = environment(mirai))
  .. <<- new.env(hash = FALSE, parent = environment(mirai))

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

  registerExternalMethods()

}

registerExternalMethods <- function() {

  ns <- .getNamespace("parallel")
  `[[<-`(
    `[[<-`(
      `[[<-`(
        ns[[".__S3MethodsTable__."]],
        "recvData.miraiNode", recvData.miraiNode
      ), "sendData.miraiNode", sendData.miraiNode
    ), "recvOneData.miraiCluster", recvOneData.miraiCluster
  )
  regs <- rbind(ns[[".__NAMESPACE__."]][["S3methods"]],
                c("recvData", "miraiNode", "recvData.miraiNode", NA_character_),
                c("sendData", "miraiNode", "sendData.miraiNode", NA_character_),
                c("recvOneData", "miraiCluster", "recvOneData.miraiCluster", NA_character_))
  `[[<-`(ns[[".__NAMESPACE__."]], "S3methods", regs)

  ns <- .getNamespace("promises")
  if (is.null(ns))
    ns <- tryCatch(loadNamespace("promises"), error = function(e) NULL)
  if (is.environment(ns)) {
    `[[<-`(ns[[".__S3MethodsTable__."]], "as.promise.mirai", as.promise.mirai)
    regs <- rbind(ns[[".__NAMESPACE__."]][["S3methods"]],
                  c("as.promise", "mirai", "as.promise.mirai", NA_character_))
    `[[<-`(ns[[".__NAMESPACE__."]], "S3methods", regs)
  }

}

# nocov end

. <- NULL
.. <- NULL
.command <- NULL
.urlscheme <- NULL

.timelimit <- 5000L
.intmax <- .Machine[["integer.max"]]
.messages <- list2env(
  list(
    arglen = "'args' and/or 'url' must be of length 1 or the same length",
    cluster_inactive = "cluster is no longer active",
    correct_context = "must be called in the correct context e.g. as a function argument",
    daemons_unset = "a numeric value for 'url' requires daemons to be set",
    dot_required = "'.' must be an element of the character vector(s) supplied to 'args'",
    missing_expression = "missing expression, perhaps wrap in {}?",
    missing_url = "at least one URL must be supplied for 'url' or 'n' must be at least 1",
    n_one = "'n' must be 1 or greater if specified with 'url'",
    n_zero = "the number of daemons must be zero or greater",
    nodes_failed = "one or more nodes failed... cluster stopped",
    numeric_n = "'n' must be numeric, did you mean to provide 'url'?",
    requires_local = "SSH tunnelling requires 'url' hostname to be '127.0.0.1' or 'localhost'",
    single_url = "only one 'url' should be specified",
    sync_timeout = "initial sync with dispatcher timed out after 5s",
    url_spec = "numeric value for 'url' is out of bounds",
    wrong_dots = "'...' arguments should only be of integer, numeric or logical type"
  ),
  hash = TRUE
)

as.promise <- NULL
recvData <- NULL
recvOneData <- NULL
sendData <- NULL
