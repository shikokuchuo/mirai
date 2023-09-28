# Copyright (C) 2023 Hibiki AI Limited <info@hibiki-ai.com>
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

# mirai.parallel ---------------------------------------------------------------

#' Make Mirai Cluster
#'
#' \code{make_cluster} creates a cluster of type 'miraiCluster', which may be
#'     used as a cluster object for any function in the \pkg{parallel} base
#'     package such as \code{\link[parallel]{clusterApply}} or
#'     \code{\link[parallel]{parLapply}}.
#'
#' @param n if specified, the integer number of local nodes to launch.
#' @param url if specified, the character URL on the host for remote nodes to
#'     dial into, including a port accepting incoming connections, e.g.
#'     'tcp://192.168.0.2:5555'. Specify a URL starting 'tls+tcp://' for secure
#'     TLS connections.
#' @param ssh (if 'url' is specified) a named list with 'nodes' being a character
#'     vector of hostnames or IP addresses of remote machines on which to launch
#'     nodes, e.g. \code{c('192.168.0.1', 'nodename')}, and optionally 'port' as
#'     the numeric port number on which to connect (defaults to standard SSH port
#'     22 if not specified).
#' @param port (if 'url' is specified) the port over which to connect via SSH
#'     (defaults to 22).
#' @param ... additional arguments passed onto \code{\link{daemons}}.
#'
#' @return For \strong{make_cluster}: An object of class 'miraiCluster' and
#'     'cluster'. Each 'miraiCluster' has an automatically assigned ID and 'n'
#'     nodes of class 'miraiNode'.
#'
#'     For \strong{stop_cluster}: invisible NULL.
#'
#' @details The defaults correspond most closely to existing usage in the
#'     \pkg{parallel} package, although '...' arguments are passed onto
#'     \code{\link{daemons}} for additional customisation if desired.
#'
#'     Note: requires R >= 4.4 (currently R-devel). Methods will not work with
#'     prior R versions.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' cl <- make_cluster(2)
#' cl
#' cl[[1L]]
#' stop_cluster(cl)
#'
#' cl <- tryCatch(
#'   make_cluster(
#'     # host URL accessible to nodes
#'     url = "tcp://10.75.37.40:5555",
#'     # node IP / hostnames to connect to with custom port number
#'     ssh = list(nodes = c("10.75.37.90", "10.75.37.91"), port = 222)
#'   )
#' , error = identity)
#' if (inherits(cl, "cluster")) stop_cluster(cl)
#'
#' }
#'
#' @export
#'
make_cluster <- function(n, url = NULL, ssh = list(nodes = character(), port = 22), ...) {

  id <- sprintf("`%d`", length(..))

  if (is.character(url)) {

    nodes <- ssh[["nodes"]]

    if (length(nodes)) {
      port <- if (length(ssh[["port"]])) as.character(ssh[["port"]]) else "22"
      daemons(url = url, dispatcher = FALSE, resilience = FALSE, cleanup = 0L, ..., .compute = id)
      for (node in nodes)
        launch_remote(1L, command = "ssh", args = c("-p", port, node, .), .compute = id)
      n <- length(nodes)

    } else {
      missing(n) && stop(.messages[["requires_n"]])
      daemons(url = url, dispatcher = FALSE, resilience = FALSE, cleanup = 0L, ..., .compute = id)
      message(sprintf("%d nodes connecting to '%s' should be launched manually", n, url))
    }

  } else {
    missing(n) && stop(.messages[["missing_url"]])
    is.numeric(n) || stop(.messages[["numeric_n"]])
    daemons(n = n, dispatcher = FALSE, resilience = FALSE, cleanup = 0L, ..., .compute = id)
  }

  pipe_notify(..[[id]][["sock"]], cv = ..[[id]][["cv"]], add = FALSE, remove = TRUE, flag = TRUE)

  cl <- vector(mode = "list", length = n)
  for (i in seq_along(cl))
    cl[[i]] <- `attributes<-`(new.env(), list(class = "miraiNode", node = i, id = id))
  reg.finalizer(.subset2(cl, 1L), stop_cluster)

  `attributes<-`(cl, list(class = c("miraiCluster", "cluster"), id = id))

}

#' Stop Mirai Cluster
#'
#' \code{stop_cluster} stops a cluster created by \code{make_cluster}.
#'
#' @param cl a 'miraiCluster'.
#'
#' @rdname make_cluster
#' @export
#'
stop_cluster <- function(cl) {

  daemons(0L, .compute = attr(cl, "id"))
  invisible()

}

#' @method stopCluster miraiCluster
#' @export
#'
stopCluster.miraiCluster <- stop_cluster

#' @method sendData miraiNode
#' @export
#'
sendData.miraiNode <- function(node, data) {

  length(..[[attr(node, "id")]]) || stop(.messages[["cluster_inactive"]])

  value <- data[["data"]]
  has_tag <- !is.null(value[["tag"]])

  node[["mirai"]] <- mirai(do.call(node, data, quote = TRUE), node = value[["fun"]], data = value[["args"]],
                           .signal = has_tag, .compute = attr(node, "id"))

  if (has_tag)
    assign("tag", value[["tag"]], node[["mirai"]])

}

#' @method recvData miraiNode
#' @export
#'
recvData.miraiNode <- function(node) call_mirai(node[["mirai"]])

#' @method recvOneData miraiCluster
#' @export
#'
recvOneData.miraiCluster <- function(cl) {

  envir <- ..[[attr(cl, "id")]]

  wait(envir[["cv"]]) || {
    stop_cluster(cl)
    stop(.messages[["nodes_failed"]])
  }

  res <- !as.logical(lapply(cl, function(x) unresolved(x[["mirai"]])))
  node <- which.max(res)
  envir <- cl[[node]][["mirai"]]
  out <- list(node = node, value = list(value = envir[["value"]], tag = envir[["tag"]]))
  assign("value", `class<-`(new.env(hash = FALSE), "unresolvedValue"), envir)
  out

}

#' @export
#'
print.miraiCluster <- function(x, ...) {

  cat(sprintf("< miraiCluster >\n - cluster ID: %s\n - nodes: %d\n - active: %s\n",
              attr(x, "id"), length(x), as.logical(length(..[[attr(x, "id")]]))), file = stdout())
  invisible(x)

}

#' @export
#'
print.miraiNode <- function(x, ...) {

  cat(sprintf("< miraiNode >\n - node: %d\n - cluster ID: %s\n", attr(x, "node"), attr(x, "id")), file = stdout())
  invisible(x)

}
