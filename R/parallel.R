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

# mirai x parallel -------------------------------------------------------------

# nocov start
# tested manually in tests/parallel/parallel-tests.R

#' Make Mirai Cluster
#'
#' \code{make_cluster} creates a cluster of type 'miraiCluster', which may be
#'     used as a cluster object for any function in the \pkg{parallel} base
#'     package such as \code{\link[parallel]{clusterApply}} or
#'     \code{\link[parallel]{parLapply}}.
#'
#' @param n integer number of nodes (launched on the local machine unless 'url'
#'     is specified).
#' @param url (specify for remote nodes), the character URL on the host for
#'     remote nodes to dial into, including a port accepting incoming connections,
#'     e.g. 'tcp://10.75.37.40:5555'. Specify a URL starting 'tls+tcp://' to use
#'     secure TLS connections.
#' @param ssh_nodes (if 'url' is specified, for launching remote nodes via SSH)
#'     a character vector of hostnames or IP addresses of the remote machines on
#'     which to launch nodes, e.g. \code{c('10.75.37.90', 'nodename')}.
#' @param ssh_port [default 22] numeric port number on which to connect.
#' @param ssh_timeout [default 5] maximum time allowed for connection setup in
#'     seconds.
#' @param ssh_tunnel [default FALSE] logical value whether to use SSH reverse
#'     tunnelling. If TRUE, a tunnel is created between the same local and
#'     remote port specified as part of 'url'. 'url' in this case must be either
#'     'localhost' or '127.0.0.1'. This is as the host listens at 'url' on the
#'     local machine, whilst the nodes dial into the same 'url' on the remote
#'     machine, and the SSH tunnel connects both ends.
#' @param ... additional arguments passed onto \code{\link{daemons}}.
#'
#' @return For \strong{make_cluster}: An object of class 'miraiCluster' and
#'     'cluster'. Each 'miraiCluster' has an automatically assigned ID and 'n'
#'     nodes of class 'miraiNode'.
#'
#'     For \strong{stop_cluster}: invisible NULL.
#'
#' @details The behaviour of clusters created by this function is designed to be
#'     as close as possible to clusters created by the \pkg{parallel} package.
#'     However, '...' arguments are passed onto \code{\link{daemons}} for
#'     additional customisation if desired.
#'
#'     For remote nodes, the 'ssh_' arguments are an optional convenience
#'     feature. If used, the number of nodes is inferred from the length of the
#'     character vector 'ssh_nodes' and 'n' is disregarded if supplied. For ease
#'     of use, SSH tunnelling assumes the same ports are available for forwarding
#'     on both host and nodes, whilst this is not strictly necessary.
#'
#'     Alternatively, by specifying 'url' and 'n', nodes may also be launched by
#'     other more customisable means, for example using \code{\link{launch_remote}}.
#'
#' @note Requires R >= 4.4 (currently R-devel). Clusters created with this
#'     function will not work with prior R versions.
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
make_cluster <- function(n, url = NULL, ssh_nodes = character(), ssh_port = 22,
                         ssh_timeout = 5, ssh_tunnel = FALSE, ...) {

  id <- sprintf("`%d`", length(..))

  if (is.character(url)) {

    if (length(ssh_nodes)) {
      daemons(url = url, dispatcher = FALSE, resilience = FALSE, cleanup = 0L, ..., .compute = id)
      for (node in ssh_nodes)
        launch_remote(
          url = 1L,
          command = "ssh",
          args = c(
            if (ssh_tunnel) {
              purl <- parse_url(url)
              purl[["hostname"]] %in% c("localhost", "127.0.0.1") || stop(.messages[["tunnel_requires"]])
              sprintf("-R %s:%s", purl[["port"]], purl[["host"]])
            },
            sprintf("-o ConnectTimeout=%s -fTp", as.character(ssh_timeout)),
            as.character(ssh_port),
            node,
            .
          ),
          .compute = id
        )
      n <- length(ssh_nodes)

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
  reg.finalizer(cl[[1L]], stop_cluster, TRUE)

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
recvData.miraiNode <- function(node) call_mirai(.subset2(node, "mirai"))

#' @method recvOneData miraiCluster
#' @export
#'
recvOneData.miraiCluster <- function(cl) {

  envir <- ..[[attr(cl, "id")]]

  wait(envir[["cv"]]) || {
    stop_cluster(cl)
    stop(.messages[["nodes_failed"]])
  }

  node <- which.min(lapply(cl, node_unresolved))
  m <- .subset2(.subset2(cl, node), "mirai")
  out <- list(node = node, value = list(value = .subset2(m, "value"), tag = .subset2(m, "tag")))
  assign("value", .unresolved_marker, m)
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

# internals --------------------------------------------------------------------

node_unresolved <- function(node) unresolved(.subset2(node, "mirai"))

# nocov end
