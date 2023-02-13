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

# queue ------------------------------------------------------------------------

#' mirai Server Queue
#'
#' Implements an active queue / task scheduler, launching and directing a
#'     cluster of daemons.
#'
#' @inheritParams server
#' @inheritParams daemons
#'
#' @return Invisible NULL.
#'
#' @section About:
#'
#'     The network topology is such that this server queue dials into the client,
#'     which listens at the '.url' address. A server queue launches a cluster of
#'     'n' daemons, and relays messages back and forth from the client. A server
#'     queue may be used in combination with other servers or server queues and
#'     the client automatically distributes tasks to all available resources.
#'
#' @export
#'
serverq <- function(n, .url) {

  sock <- socket(protocol = "rep")
  on.exit(expr = close(sock))
  dial(sock, url = .url) && stop()
  queue <- vector(mode = "list", length = n)
  cluster <- vector(mode = "list", length = n)

  for (i in seq_len(n)) {
    url <- sprintf(.urlfmt, random())
    socko <- socket(protocol = "req", listen = url)
    system2(command = .command,
            args = c("--vanilla", "-e", shQuote(sprintf("mirai::server(%s)", deparse(url)))),
            stdout = NULL, stderr = NULL, wait = FALSE)
    cluster[[i]] <- list(url = url, sock = socko, free = TRUE)
    ctx <- context(sock)
    req <- recv_aio(ctx, mode = 1L)
    queue[[i]] <- list(ctx = ctx, req = req)
  }

  on.exit(expr = for (i in seq_len(n)) {
    send(cluster[[i]][["sock"]], data = .__scm__., mode = 2L)
    close(cluster[[i]][["sock"]])
  }, add = TRUE)

  repeat {

    free <- which(unlist(lapply(cluster, .subset2, "free")))

    msleep(if (length(free) == n) 50L else 5L)

    if (length(free))
      for (q in free)
        for (i in seq_len(n))
          if (length(queue[[i]]) == 2L && !unresolved(queue[[i]][["req"]])) {
            ctx <- context(cluster[[q]][["sock"]])
            queue[[i]][["rctx"]] <- ctx
            queue[[i]][["res"]] <- request(ctx, data = queue[[i]][["req"]][["data"]], send_mode = 1L, recv_mode = 1L)
            queue[[i]][["daemon"]] <- q
            cluster[[q]][["free"]] <- FALSE
            break
          }

    for (i in seq_len(n))
      if (length(queue[[i]]) > 2L && !unresolved(queue[[i]][["res"]])) {
        send(queue[[i]][["ctx"]], data = queue[[i]][["res"]][["data"]], mode = 1L)
        q <- queue[[i]][["daemon"]]
        cluster[[q]][["free"]] <- TRUE
        ctx <- context(sock)
        req <- recv_aio(ctx, mode = 1L)
        queue[[i]] <- list(ctx = ctx, req = req)
      }

  }

}

