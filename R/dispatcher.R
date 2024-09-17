# Copyright (C) 2023-2024 Hibiki AI Limited <info@hibiki-ai.com>
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
#' Dispatches tasks from a host to daemons for processing, using FIFO
#'     scheduling, queuing tasks as required. Daemon / dispatcher settings
#'     may be controlled by \code{\link{daemons}} and this function should not
#'     need to be invoked directly.
#'
#' @inheritParams daemon
#' @param host the character host URL to dial (where tasks are sent from),
#'     including the port to connect to (and optionally for websockets, a path),
#'     e.g. 'tcp://hostname:5555' or 'ws://10.75.32.70:5555/path'.
#' @param url (optional) the character URL or vector of URLs dispatcher should
#'     listen at, including the port to connect to (and optionally for websockets,
#'     a path), e.g. 'tcp://hostname:5555' or 'ws://10.75.32.70:5555/path'.
#'     Specify 'tls+tcp://' or 'wss://' to use secure TLS connections. Tasks are
#'     sent to daemons dialled into these URLs. If not supplied, \sQuote{n}
#'     local inter-process URLs will be assigned automatically.
#' @param n (optional) if specified, the integer number of daemons to listen for.
#'     Otherwise \sQuote{n} will be inferred from the number of URLs supplied in
#'     \sQuote{url}. Where a single URL is supplied and \sQuote{n} > 1,
#'     \sQuote{n} unique URLs will be automatically assigned for daemons to dial
#'     into.
#' @param ... (optional) additional arguments passed through to
#'     \code{\link{daemon}}. These include  \sQuote{asyncdial},
#'     \sQuote{autoexit}, \sQuote{cleanup}, \sQuote{maxtasks},
#'     \sQuote{idletime}, \sQuote{walltime} and \sQuote{timerstart}.
#' @param retry [default FALSE] logical value, whether to automatically retry
#'     tasks where the daemon crashes or terminates unexpectedly on the next
#'     daemon instance to connect. If TRUE, the mirai will remain unresolved but
#'     \code{\link{status}} will show \sQuote{online} as 0 and \sQuote{assigned}
#'     > \sQuote{complete}. To cancel a task in this case, use
#'     \code{saisei(force = TRUE)}. If FALSE, such tasks will be returned as
#'     \sQuote{errorValue} 19 (Connection reset).
#' @param token [default FALSE] if TRUE, appends a unique 24-character token
#'     to each URL path the dispatcher listens at (not applicable for TCP URLs
#'     which do not accept a path).
#' @param tls [default NULL] (required for secure TLS connections)
#'     \strong{either} the character path to a file containing the PEM-encoded
#'     TLS certificate and associated private key (may contain additional
#'     certificates leading to a validation chain, with the TLS certificate
#'     first), \strong{or} a length 2 character vector comprising [i] the TLS
#'     certificate (optionally certificate chain) and [ii] the associated
#'     private key.
#' @param pass [default NULL] (required only if the private key supplied to
#'     \sQuote{tls} is encrypted with a password) For security, should be
#'     provided through a function that returns this value, rather than directly.
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
dispatcher <- function(host, url = NULL, n = NULL, ..., retry = FALSE, token = FALSE,
                       tls = NULL, pass = NULL, rs = NULL, monitor = NULL) {

  n <- if (is.numeric(n)) as.integer(n) else length(url)
  n > 0L || stop(._[["missing_url"]])

  cv <- cv()
  sock <- socket(protocol = "rep")
  on.exit(reap(sock))
  pipe_notify(sock, cv = cv, remove = TRUE, flag = TRUE)
  dial_and_sync_socket(sock = sock, url = host)

  ctrchannel <- is.character(monitor)
  if (ctrchannel) {
    sockc <- socket(protocol = "rep")
    on.exit(reap(sockc), add = TRUE, after = FALSE)
    pipe_notify(sockc, cv = cv, remove = TRUE, flag = TRUE)
    dial_and_sync_socket(sock = sockc, url = monitor)
    cmessage <- recv(sockc, mode = 2L, block = .limit_long)
    is.object(cmessage) && stop(._[["sync_dispatcher"]])
    if (nzchar(cmessage[2L]))
      Sys.setenv(R_DEFAULT_PACKAGES = cmessage[2L]) else
        Sys.unsetenv("R_DEFAULT_PACKAGES")
  }

  auto <- is.null(url)
  vectorised <- length(url) == n
  seq_n <- seq_len(n)
  basenames <- servernames <- character(n)
  activestore <- instance <- complete <- assigned <- integer(n)
  serverfree <- !integer(n)
  active <- servers <- queue <- vector(mode = "list", length = n)
  if (auto) {
    dots <- parse_dots(...)
    output <- attr(dots, "output")
  } else {
    ports <- get_ports(url = url, n = n)
    if (length(ports)) token <- FALSE
    if (ctrchannel && nzchar(cmessage[4L]) && is.null(tls)) {
      tls <- c(cmessage[4L], if (nzchar(cmessage[6L])) cmessage[6L])
      pass <- if (nzchar(cmessage[8L])) cmessage[8L]
    }
    if (length(tls))
      tls <- tls_config(server = tls, pass = pass)
  }
  pass <- NULL

  envir <- new.env(hash = FALSE)
  if (is.numeric(rs)) `[[<-`(envir, "stream", as.integer(rs))

  for (i in seq_n) {
    burl <- if (auto) .urlscheme else
      if (vectorised) url[i] else
        if (is.null(ports)) sprintf(if (startsWith(url, "ipc")) "%s-%d" else "%s/%d", url, i) else
          sub(ports[1L], ports[i], url, fixed = TRUE)
    nurl <- if (auto) local_url() else if (token) tokenized_url(burl) else burl
    ncv <- cv()
    nsock <- req_socket(NULL, resend = retry * .intmax)
    pipe_notify(nsock, cv = ncv, cv2 = cv, add = TRUE, remove = TRUE)
    lock(nsock, cv = ncv)
    listen(nsock, url = nurl, tls = tls, error = TRUE)
    listener <- attr(nsock, "listener")[[1L]]
    listurl <- opt(listener, "url")
    if (i == 1L && !auto && parse_url(listurl)[["port"]] == "0") {
      realport <- opt(listener, "tcp-bound-port")
      listurl <- sub_real_port(port = realport, url = nurl)
      if (!vectorised || n == 1L) {
        url <- sub_real_port(port = realport, url = url)
        burl <- sub_real_port(port = realport, url = burl)
      }
    }

    auto && launch_daemon(wa3(nurl, dots, next_stream(envir)), output)

    basenames[i] <- burl
    servernames[i] <- listurl
    servers[[i]] <- nsock
    active[[i]] <- ncv
    queue[[i]] <- create_req(ctx = .context(sock), cv = cv)
  }

  on.exit(lapply(servers, reap), add = TRUE, after = TRUE)

  if (auto)
    for (i in seq_n)
      until(cv, .limit_long) || stop(._[["sync_daemons"]])

  if (ctrchannel) {
    send(sockc, c(Sys.getpid(), servernames), mode = 2L)
    cmessage <- recv_aio(sockc, mode = 5L, cv = cv)
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
        i <- .subset2(cmessage, "value")
        if (i) {
          if (i > 0L && !activevec[[i]]) {
            reap(attr(servers[[i]], "listener")[[1L]])
            attr(servers[[i]], "listener") <- NULL
            data <- servernames[i] <- if (auto) local_url() else tokenized_url(basenames[i])
            instance[i] <- -abs(instance[i])
            listen(servers[[i]], url = data, tls = tls, error = TRUE)

          } else if (i < 0L) {
            i <- -i
            reap(servers[[i]])
            servers[[i]] <- nsock <- req_socket(NULL, resend = retry * .intmax)
            pipe_notify(nsock, cv = active[[i]], cv2 = cv, add = TRUE, remove = TRUE)
            lock(nsock, cv = active[[i]])
            data <- servernames[i] <- if (auto) local_url() else tokenized_url(basenames[i])
            instance[i] <- -abs(instance[i])
            listen(nsock, url = data, tls = tls, error = TRUE)

          } else {
            data <- ""

          }
        } else {
          data <- as.integer(c(seq_n, activevec, instance, assigned, complete))
        }
        send(sockc, data = data, mode = 2L)
        cmessage <- recv_aio(sockc, mode = 5L, cv = cv)
        next
      }

      for (i in seq_n)
        if (length(queue[[i]]) > 2L && !unresolved(queue[[i]][["req"]])) {
          req <- .subset2(queue[[i]][["req"]], "value")
          if (is.object(req)) req <- serialize(req, connection = NULL, xdr = FALSE)
          send(queue[[i]][["ctx"]], data = req, mode = 2L, block = TRUE)
          q <- queue[[i]][["daemon"]]
          if (req[4L]) {
            send(queue[[i]][["rctx"]], NULL, mode = 2L, block = TRUE)
            reap(queue[[i]][["rctx"]])
          } else {
            serverfree[q] <- TRUE
          }
          complete[q] <- complete[q] + 1L
          queue[[i]] <- create_req(ctx = .context(sock), cv = cv)
        }

      free <- which(serverfree & activevec)

      if (length(free))
        for (q in free)
          for (i in seq_n) {
            if (length(queue[[i]]) == 2L && !unresolved(queue[[i]][["req"]])) {
              queue[[i]][["rctx"]] <- .context(servers[[q]])
              queue[[i]][["req"]] <- request(queue[[i]][["rctx"]], data = .subset2(queue[[i]][["req"]], "value"),
                                             send_mode = 2L, recv_mode = 8L, cv = cv)
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
#' @inheritParams mirai
#' @param i integer index number URL to regenerate at dispatcher.
#' @param force [default FALSE] logical value whether to regenerate the URL even
#'     when there is an existing active connection.
#'
#' @return The regenerated character URL upon success, or else NULL.
#'
#' @details When a URL is regenerated, the listener at the specified socket is
#'     closed and replaced immediately, hence this function will only be
#'     successful if there are no existing connections at the socket (i.e.
#'     'online' status shows 0), unless the argument \sQuote{force} is specified
#'     as TRUE.
#'
#'     If \sQuote{force} is specified as TRUE, the socket is immediately closed
#'     and regenerated. If this happens while a mirai task is still ongoing, it
#'     will be returned as an \sQuote{errorValue} 7 (Object closed). This may be
#'     used to cancel a task that consistently hangs or crashes to prevent it
#'     from failing repeatedly when new daemons connect.
#'
#' @section Timeouts:
#'
#'     Specifying the '.timeout' argument to \code{\link{mirai}} ensures that
#'     the 'mirai' always resolves. However, the task may not have completed and
#'     still be ongoing in the daemon process. In such situations, dispatcher
#'     ensures that queued tasks are not assigned to the busy process, however
#'     overall performance may still be degraded if they remain in use.
#'
#'     If a process hangs and cannot be restarted otherwise, \code{saisei}
#'     specifying \code{force = TRUE} may be used to cancel the task and
#'     regenerate any particular URL for a new \code{\link{daemon}} to connect
#'     to.
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
  i <- as.integer(i[1L])
  length(envir[["sockc"]]) && i > 0L && i <= envir[["n"]] && !startsWith(envir[["urls"]][i], "t") || return()
  r <- query_dispatcher(sock = envir[["sockc"]], command = if (force) -i else i, mode = 9L)
  is.character(r) && nzchar(r) || return()
  envir[["urls"]][i] <- r
  r

}

# internals --------------------------------------------------------------------

get_ports <- function(url, n) {
  baseurl <- parse_url(url)
  if (startsWith(baseurl[["scheme"]], "t")) {
    if (baseurl[["port"]] == "0") integer(n) else seq.int(baseurl[["port"]], length.out = n)
  }
}

sub_real_port <- function(port, url) sub("(?<=:)0(?![^/])", port, url, perl = TRUE)

query_dispatcher <- function(sock, command, mode, block = .limit_short)
  if (r <- send(sock, data = command, mode = 2L, block = block)) r else
    recv(sock, mode = mode, block = block)

create_req <- function(ctx, cv)
  list(ctx = ctx, req = recv_aio(ctx, mode = 8L, cv = cv))
