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

# mirai ------------------------------------------------------------------------

#' Dispatcher
#'
#' Implements a dispatcher for tasks from a host to multiple daemons for
#'     processing, using a FIFO scheduling rule, queuing tasks as required.
#'
#' @inheritParams daemon
#' @param host the character host URL to dial (where tasks are sent from),
#'     including the port to connect to (and optionally for websockets, a path),
#'     e.g. 'tcp://10.75.32.70:5555' or 'ws://10.75.32.70:5555/path'.
#' @param url (optional) the character URL or vector of URLs dispatcher should
#'     listen at, including the port to connect to (and optionally for websockets,
#'     a path), e.g. 'tcp://10.75.32.70:5555' or 'ws://10.75.32.70:5555/path'.
#'     Specify 'tls+tcp://' or 'wss://' to use secure TLS connections. Tasks are
#'     sent to daemons dialled into these URLs. If not supplied, 'n' local
#'     inter-process URLs will be assigned automatically.
#' @param n (optional) if specified, the integer number of daemons to listen for.
#'     Otherwise 'n' will be inferred from the number of URLs supplied in 'url'.
#'     Where a single URL is supplied and 'n' > 1, 'n' unique URLs will be
#'     automatically assigned for daemons to dial into.
#' @param ... (optional) additional arguments passed through to \code{\link{daemon}}.
#'     These include 'maxtasks', 'idletime', 'walltime', 'timerstart', and
#'     'cleanup'.
#' @param token [default FALSE] if TRUE, appends a unique 40-character token
#'     to each URL path the dispatcher listens at (not applicable for TCP URLs
#'     which do not accept a path).
#' @param lock [default FALSE] if TRUE, sockets lock once a connection has been
#'     accepted, preventing further connection attempts. This provides safety
#'     against more than one daemon attempting to connect to a unique URL.
#' @param tls [default NULL] (required for secure TLS connections) \strong{either}
#'     the character path to a file containing the PEM-encoded TLS certificate
#'     and associated private key (may contain additional certificates leading
#'     to a validation chain, with the TLS certificate first), \strong{or} a
#'     length 2 character vector comprising [i] the TLS certificate (optionally
#'     certificate chain) and [ii] the associated private key.
#' @param pass [default NULL] (required only if the private key supplied to 'tls'
#'     is encrypted with a password) For security, should be provided through a
#'     function that returns this value, rather than directly.
#' @param monitor (for package internal use only) do not set this parameter.
#'
#' @return Invisible NULL.
#'
#' @details The network topology is such that a dispatcher acts as a gateway
#'     between the host and daemons, ensuring that tasks received from the host
#'     are dispatched on a FIFO basis for processing. Tasks are queued at the
#'     dispatcher to ensure tasks are only sent to daemons that can begin
#'     immediate execution of the task.
#'
#' @export
#'
dispatcher <- function(host, url = NULL, n = NULL, ...,
                       asyncdial = FALSE, token = FALSE, lock = FALSE,
                       tls = NULL, pass = NULL, rs = NULL, monitor = NULL) {

  n <- if (is.numeric(n)) as.integer(n) else length(url)
  n > 0L || stop(.messages[["missing_url"]])

  sock <- socket(protocol = "rep")
  on.exit(reap(sock))
  cv <- cv()
  pipe_notify(sock, cv = cv, add = FALSE, remove = TRUE, flag = TRUE)
  dial_and_sync_socket(sock = sock, url = host, asyncdial = asyncdial)

  auto <- is.null(url)
  vectorised <- length(url) == n
  seq_n <- seq_len(n)
  basenames <- servernames <- character(n)
  activestore <- instance <- complete <- assigned <- integer(n)
  serverfree <- !integer(n)
  active <- servers <- queue <- vector(mode = "list", length = n)
  if (auto) {
    dots <- parse_dots(...)
  } else {
    baseurl <- parse_url(url)
    if (substr(baseurl[["scheme"]], 1L, 1L) == "t") {
      ports <- if (baseurl[["port"]] == "0") integer(n) else seq.int(baseurl[["port"]], length.out = n)
      token <- FALSE
    } else {
      ports <- NULL
    }

    if (substr(baseurl[["scheme"]], 1L, 3L) %in% c("wss", "tls") && is.null(tls)) {
      tls <- get_and_reset_env("MIRAI_TEMP_FIELD1")
      if (length(tls))
        tls <- c(tls, get_and_reset_env("MIRAI_TEMP_FIELD2"))
    }
    if (length(tls)) {
      if (is.null(pass))
        pass <- get_and_reset_env("MIRAI_TEMP_VAR")
      tls <- tls_config(server = tls, pass = pass)
      pass <- NULL
    }
  }

  envir <- ..[["default"]]
  if (length(rs)) `[[<-`(envir, "stream", as.integer(rs))

  for (i in seq_n) {
    burl <- if (auto) .urlscheme else
      if (vectorised) url[i] else
        if (is.null(ports)) sprintf("%s/%d", url, i) else
          sub(ports[1L], ports[i], url, fixed = TRUE)
    basenames[i] <- burl
    nurl <- if (auto) auto_tokenized_url() else if (token) new_tokenized_url(burl) else burl
    nsock <- req_socket(NULL)
    ncv <- cv()
    pipe_notify(nsock, cv = ncv, cv2 = cv, flag = FALSE)
    listen(nsock, url = nurl, tls = tls, error = TRUE)
    lock && lock(nsock, cv = ncv)
    listener <- attr(nsock, "listener")[[1L]]
    if (i == 1L && !auto && parse_url(opt(listener, "url"))[["port"]] == "0") {
      realport <- opt(listener, "tcp-bound-port")
      servernames[i] <- sub_real_port(port = realport, url = nurl)
      if (!vectorised || n == 1L) {
        url <- sub_real_port(port = realport, url = url)
        basenames[1L] <- sub_real_port(port = realport, url = burl)
      }
    } else {
      servernames[i] <- opt(listener, "url")
    }

    auto && launch_daemon(nurl, dots, next_stream(envir))

    servers[[i]] <- nsock
    active[[i]] <- ncv
    ctx <- .context(sock)
    req <- recv_aio_signal(ctx, cv = cv, mode = 8L)
    queue[[i]] <- list(ctx = ctx, req = req)
  }

  on.exit(lapply(servers, reap), add = TRUE, after = TRUE)

  ctrchannel <- is.character(monitor)
  if (ctrchannel) {
    sockc <- socket(protocol = "rep")
    on.exit(reap(sockc), add = TRUE, after = FALSE)
    pipe_notify(sockc, cv = cv, add = FALSE, remove = TRUE, flag = TRUE)
    dial_and_sync_socket(sock = sockc, url = monitor, asyncdial = asyncdial)
    recv(sockc, mode = 5L, block = .timelimit) && stop(.messages[["sync_timeout"]])
    send_aio(sockc, c(Sys.getpid(), servernames), mode = 2L)
    cmessage <- recv_aio_signal(sockc, cv = cv, mode = 5L)
  }

  suspendInterrupts(
    repeat {

      wait(cv) || break

      cv_values <- as.integer(lapply(active, cv_value))
      activevec <- cv_values %% 2L
      changes <- (activevec - activestore) > 0L
      activestore <- activevec
      if (any(changes)) {
        instance[changes] <- abs(instance[changes]) + 1L
        serverfree <- serverfree | changes
      }

      ctrchannel && !unresolved(cmessage) && {
        i <- .subset2(cmessage, "data")
        if (i) {
          if (i > 0L && !activevec[[i]]) {
            reap(attr(servers[[i]], "listener")[[1L]])
            attr(servers[[i]], "listener") <- NULL
            data <- servernames[i] <- if (auto) auto_tokenized_url() else new_tokenized_url(basenames[i])
            instance[i] <- -abs(instance[i])
            listen(servers[[i]], url = data, tls = tls, error = TRUE)

          } else if (i < 0L) {
            i <- -i
            reap(servers[[i]])
            servers[[i]] <- nsock <- req_socket(NULL)
            pipe_notify(nsock, cv = active[[i]], cv2 = cv, flag = FALSE)
            data <- servernames[i] <- if (auto) auto_tokenized_url() else new_tokenized_url(basenames[i])
            instance[i] <- -abs(instance[i])
            listen(nsock, url = data, tls = tls, error = TRUE)
            lock && lock(nsock, cv = active[[i]])

          } else {
            data <- ""

          }
        } else {
          data <- as.integer(c(seq_n, activevec, instance, assigned, complete))
        }
        send_aio(sockc, data = data, mode = 2L)
        cmessage <- recv_aio_signal(sockc, cv = cv, mode = 5L)
        next
      }

      for (i in seq_n)
        if (length(queue[[i]]) > 2L && !unresolved(queue[[i]][["res"]])) {
          req <- .subset2(queue[[i]][["res"]], "value")
          if (is.object(req)) req <- serialize(req, NULL)
          send(queue[[i]][["ctx"]], data = req, mode = 2L)
          q <- queue[[i]][["daemon"]]
          if (req[1L] == .next_format_identifier) {
            ctx <- .context(servers[[q]])
            send_aio(ctx, data = .next_format_identifier, mode = 2L)
            reap(ctx)
          } else {
            serverfree[q] <- TRUE
          }
          complete[q] <- complete[q] + 1L
          ctx <- .context(sock)
          req <- recv_aio_signal(ctx, cv = cv, mode = 8L)
          queue[[i]] <- list(ctx = ctx, req = req)
        }

      free <- which(serverfree & activevec)

      if (length(free))
        for (q in free)
          for (i in seq_n) {
            if (length(queue[[i]]) == 2L && !unresolved(queue[[i]][["req"]])) {
              queue[[i]][["res"]] <- request_signal(.context(servers[[q]]), data = .subset2(queue[[i]][["req"]], "value"), cv = cv, send_mode = 2L, recv_mode = 8L)
              queue[[i]][["daemon"]] <- q
              serverfree[q] <- FALSE
              assigned[q] <- assigned[q] + 1L
              break
            }
            serverfree[q] || break
          }

    }
  )

}

#' Saisei (Regenerate Token)
#'
#' When using daemons with dispatcher, regenerates the token for the URL a
#'     dispatcher socket listens at.
#'
#' @param i integer index number URL to regenerate at dispatcher.
#' @param force [default FALSE] logical value whether to regenerate the URL even
#'     when there is an existing active connection.
#' @param .compute [default 'default'] character compute profile (each compute
#'     profile has its own set of daemons for connecting to different resources).
#'
#' @return The regenerated character URL upon success, or else NULL.
#'
#' @details When a URL is regenerated, the listener at the specified socket is
#'     closed and replaced immediately, hence this function will only be
#'     successful if there are no existing connections at the socket (i.e.
#'     'online' status shows 0), unless the argument 'force' is specified as TRUE.
#'
#'     If 'force' is specified as TRUE, the socket is immediately closed and
#'     regenerated. If this happens while a mirai is still ongoing, it will be
#'     returned as an errorValue 7 'Object closed'. This may be used to cancel a
#'     task that consistently hangs or crashes to prevent it from failing
#'     repeatedly when new daemons connect.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' daemons(1L)
#' Sys.sleep(1L)
#' status()
#' saisei(i = 1L, force = TRUE)
#' status()
#'
#' daemons(0)
#'
#' }
#'
#' @export
#'
saisei <- function(i, force = FALSE, .compute = "default") {

  envir <- ..[[.compute]]
  i <- as.integer(`length<-`(i, 1L))
  length(envir[["sockc"]]) && i > 0L && i <= envir[["n"]] && substr(envir[["urls"]][i], 1L, 1L) != "t" || return()
  r <- query_dispatcher(sock = envir[["sockc"]], command = if (force) -i else i, mode = 9L)
  is.character(r) && nzchar(r) || return()
  envir[["urls"]][i] <- r
  r

}

# internals --------------------------------------------------------------------

auto_tokenized_url <- function() strcat(.urlscheme, random(12L))

new_tokenized_url <- function(url) sprintf("%s/%s", url, random(12L))

sub_real_port <- function(port, url) sub("(?<=:)0(?![^/])", port, url, perl = TRUE)

query_dispatcher <- function(sock, command, mode) {
  send(sock, data = command, mode = 2L, block = .timelimit)
  recv(sock, mode = mode, block = .timelimit)
}

query_status <- function(envir) {
  res <- query_dispatcher(sock = envir[["sockc"]], command = 0L, mode = 5L)
  is.object(res) && return(res)
  `attributes<-`(res, list(dim = c(envir[["n"]], 5L),
                           dimnames = list(envir[["urls"]], c("i", "online", "instance", "assigned", "complete"))))
}

init_monitor <- function(sockc, envir) {
  res <- query_dispatcher(sockc, command = 0L, mode = 2L)
  is.object(res) && stop(.messages[["sync_timeout"]])
  `[[<-`(`[[<-`(`[[<-`(envir, "sockc", sockc), "urls", res[-1L]), "pid", as.integer(res[1L]))
}

get_and_reset_env <- function(x) {
  candidate <- Sys.getenv(x)
  if (nzchar(candidate)) {
    Sys.unsetenv(x)
    candidate
  }
}
