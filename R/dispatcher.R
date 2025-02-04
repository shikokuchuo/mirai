# Copyright (C) 2023-2025 Hibiki AI Limited <info@hibiki-ai.com>
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
#' scheduling, queuing tasks as required. Daemon / dispatcher settings may be
#' controlled by \code{\link{daemons}} and this function should not need to be
#' invoked directly.
#'
#' The network topology is such that a dispatcher acts as a gateway between the
#' host and daemons, ensuring that tasks received from the host are dispatched
#' on a FIFO basis for processing. Tasks are queued at the dispatcher to ensure
#' tasks are only sent to daemons that can begin immediate execution of the
#' task.
#'
#' @inheritParams daemon
#' @param host the character host URL to dial (where tasks are sent from),
#'   including the port to connect to e.g. 'tcp://hostname:5555' or
#'   'tcp://10.75.32.70:5555'.
#' @param url (optional) the character URL dispatcher should listen at (and
#'   daemons should dial in to), including the port to connect to e.g.
#'   'tcp://hostname:5555' or 'tcp://10.75.32.70:5555'. Specify 'tls+tcp://' to
#'   use secure TLS connections.
#' @param n (optional) if specified, the integer number of daemons to launch. In
#'   this case, a local url is automatically generated.
#' @param ... (optional) additional arguments passed through to
#'   \code{\link{daemon}}. These include \sQuote{asyncdial}, \sQuote{autoexit},
#'   and \sQuote{cleanup}.
#' @param tls [default NULL] (required for secure TLS connections)
#'   \strong{either} the character path to a file containing the PEM-encoded TLS
#'   certificate and associated private key (may contain additional certificates
#'   leading to a validation chain, with the TLS certificate first), \strong{or}
#'   a length 2 character vector comprising [i] the TLS certificate (optionally
#'   certificate chain) and [ii] the associated private key.
#' @param pass [default NULL] (required only if the private key supplied to
#'   \sQuote{tls} is encrypted with a password) For security, should be provided
#'   through a function that returns this value, rather than directly.
#' @param monitor unused legacy parameter - do not specify this value.
#'
#' @return Invisible NULL.
#'
#' @export
#'
dispatcher <- function(host, url = NULL, n = NULL, ..., tls = NULL, pass = NULL,
                       rs = NULL, monitor = NULL) {

  missing(monitor) ||
    return(v1_dispatcher(host = host, url = url, n = n, ...,
                         tls = tls, pass = pass, rs = rs, monitor = monitor))

  n <- if (is.numeric(n)) as.integer(n) else length(url)
  n > 0L || stop(._[["missing_url"]])

  cv <- cv()
  sock <- socket("rep")
  on.exit(reap(sock))
  pipe_notify(sock, cv = cv, remove = TRUE, flag = TRUE)
  dial_and_sync_socket(sock, host)

  ctx <- .context(sock)
  sync <- recv(ctx, mode = 1L, block = .limit_long)
  is.object(sync) && stop(._[["sync_dispatcher"]])
  if (nzchar(sync[[1L]]))
    Sys.setenv(R_DEFAULT_PACKAGES = sync[[1L]]) else
      Sys.unsetenv("R_DEFAULT_PACKAGES")

  auto <- is.null(url)
  if (auto) {
    url <- local_url()
  } else {
    if (is.character(sync[[2L]]) && is.null(tls)) {
      tls <- sync[[2L]]
      pass <- sync[[3L]]
    }
    if (length(tls))
      tls <- tls_config(server = tls, pass = pass)
  }
  pass <- NULL
  serial <- sync[[4L]]

  psock <- socket("poly")
  on.exit(reap(psock), add = TRUE, after = TRUE)
  m <- monitor(psock, cv)
  listen(psock, url = url, tls = tls, error = TRUE)

  msgid <- 0L
  inq <- outq <- list()
  events <- integer()
  envir <- new.env(hash = FALSE)
  if (is.numeric(rs)) `[[<-`(envir, "stream", as.integer(rs))
  if (auto) {
    dots <- parse_dots(...)
    output <- attr(dots, "output")
    for (i in seq_len(n))
      launch_daemon(wa32(url, dots, next_stream(envir)), output)
    for (i in seq_len(n))
      until(cv, .limit_long) || stop(._[["sync_daemons"]])

    changes <- read_monitor(m)
    for (item in changes)
      if (item > 0) {
        outq[[as.character(item)]] <- as.environment(list(pipe = item, msgid = 0L, ctx = NULL))
        send(psock, serial, mode = 1L, block = TRUE, pipe = item)
      }

  } else {
    url <- check_url(psock)
  }

  send(ctx, c(Sys.getpid(), url), mode = 2L, block = TRUE)

  ctx <- .context(sock)
  req <- recv_aio(ctx, mode = 8L, cv = cv)
  res <- recv_aio(psock, mode = 8L, cv = cv)

  suspendInterrupts(
    repeat {

      wait(cv) || break

      changes <- read_monitor(m)
      length(changes) && {
        for (item in changes) {
          if (item > 0) {
            outq[[as.character(item)]] <- as.environment(list(pipe = item, msgid = 0L, ctx = NULL))
            send(psock, serial, mode = 1L, block = TRUE, pipe = item)
            cv_signal(cv)
          } else {
            id <- as.character(-item)
            if (length(outq[[id]])) {
              if (outq[[id]][["msgid"]])
                send(outq[[id]][["ctx"]], .connectionReset, mode = 1L, block = TRUE)
              if (length(outq[[id]][["dmnid"]]))
                events <- c(events, outq[[id]][["dmnid"]])
              outq[[id]] <- NULL
            }
          }
        }
        next
      }

      if (!unresolved(req)) {
        value <- .subset2(req, "value")

        if (value[1L] == 0L) {
          id <- readBin(value, "integer", n = 2L)[2L]
          if (id == 0L) {
            found <- c(
              length(outq),
              length(inq),
              sum(as.logical(unlist(lapply(outq, .subset2, "msgid"), use.names = FALSE))),
              events
            )
            events <- integer()
          } else {
            found <- FALSE
            for (item in outq)
              if (item[["msgid"]] == id) {
                send(psock, 0L, mode = 1L, pipe = item[["pipe"]], block = TRUE)
                `[[<-`(item, "msgid", -1L)
                found <- TRUE
                break
              }
            if (!found)
              for (i in seq_along(inq))
                if (inq[[i]][["msgid"]] == id) {
                  inq[[i]] <- NULL
                  found <- TRUE
                  break
                }
          }
          send(ctx, found, mode = 2L, block = TRUE)

        } else {
          msgid <- msgid + 1L
          inq[[length(inq) + 1L]] <- list(ctx = ctx, req = value, msgid = msgid)
        }
        ctx <- .context(sock)
        req <- recv_aio(ctx, mode = 8L, cv = cv)

      } else if (!unresolved(res)) {
        value <- .subset2(res, "value")
        id <- as.character(.subset2(res, "aio"))
        res <- recv_aio(psock, mode = 8L, cv = cv)
        if (outq[[id]][["msgid"]] < 0) {
          `[[<-`(outq[[id]], "msgid", 0L)
          cv_signal(cv)
          next
        }
        if (value[4L]) {
          if (value[4L] == 1L) {
            send(outq[[id]][["ctx"]], value, mode = 2L, block = TRUE)
            send(psock, 0L, mode = 2L, pipe = outq[[id]][["pipe"]], block = TRUE)
            if (length(outq[[id]][["dmnid"]]))
              events <- c(events, outq[[id]][["dmnid"]])
            outq[[id]] <- NULL
          } else {
            dmnid <- readBin(value, "integer", n = 2L)[2L]
            events <- c(events, dmnid)
            `[[<-`(outq[[id]], "dmnid", -dmnid)
          }
          next
        } else {
          send(outq[[id]][["ctx"]], value, mode = 2L, block = TRUE)
          `[[<-`(outq[[id]], "msgid", 0L)
        }
      }

      if (length(inq))
        for (item in outq)
          if (!item[["msgid"]]) {
            send(psock, inq[[1L]][["req"]], mode = 2L, pipe = item[["pipe"]], block = TRUE)
            `[[<-`(item, "ctx", inq[[1L]][["ctx"]])
            `[[<-`(item, "msgid", inq[[1L]][["msgid"]])
            inq[[1L]] <- NULL
            break
          }

    }
  )

}

# internals --------------------------------------------------------------------

get_ports <- function(url, n) {
  baseurl <- parse_url(url)
  if (startsWith(baseurl[["scheme"]], "t")) {
    if (baseurl[["port"]] == "0") integer(n) else seq.int(from = baseurl[["port"]], length.out = n)
  }
}

sub_real_port <- function(port, url) sub("(?<=:)0(?![^/])", port, url, perl = TRUE)

check_url <- function(sock) {
  listener <- attr(sock, "listener")[[1L]]
  url <- opt(listener, "url")
  if (parse_url(url)[["port"]] == "0")
    url <- sub_real_port(opt(listener, "tcp-bound-port"), url)
  url
}

query_dispatcher <- function(sock, command, send_mode = 2L, recv_mode = 5L, block = .limit_short)
  if (r <- send(sock, command, mode = send_mode, block = block)) r else
    recv(sock, mode = recv_mode, block = block)

create_req <- function(ctx, cv)
  list(ctx = ctx, req = recv_aio(ctx, mode = 8L, cv = cv))

# Legacy compatibility functions -----------------------------------------------

v1_dispatcher <- function(host, url = NULL, n = NULL, ..., retry = FALSE,
                          token = FALSE, tls = NULL, pass = NULL, rs = NULL, monitor = NULL) {

  n <- if (is.numeric(n)) as.integer(n) else length(url)
  n > 0L || stop(._[["missing_url"]])

  cv <- cv()
  sock <- socket("rep")
  on.exit(reap(sock))
  pipe_notify(sock, cv = cv, remove = TRUE, flag = TRUE)
  dial_and_sync_socket(sock, host)

  ctrchannel <- is.character(monitor)
  if (ctrchannel) {
    sockc <- socket("rep")
    on.exit(reap(sockc), add = TRUE, after = FALSE)
    pipe_notify(sockc, cv = cv, remove = TRUE, flag = TRUE)
    dial_and_sync_socket(sockc, monitor)
    cmessage <- recv(sockc, mode = 1L, block = .limit_long)
    is.object(cmessage) && stop(._[["sync_dispatcher"]])
    if (nzchar(cmessage[[1L]]))
      Sys.setenv(R_DEFAULT_PACKAGES = cmessage[[1L]]) else
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
    ports <- get_ports(url, n)
    if (length(ports)) token <- FALSE
    if (ctrchannel && is.character(cmessage[[2L]]) && is.null(tls)) {
      tls <- cmessage[[2L]]
      pass <- cmessage[[3L]]
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
      listurl <- sub_real_port(realport, nurl)
      if (!vectorised || n == 1L) {
        url <- sub_real_port(realport, url)
        burl <- sub_real_port(realport, burl)
      }
    }

    auto && launch_daemon(wa31(nurl, dots, next_stream(envir)), output)

    basenames[i] <- burl
    servernames[i] <- listurl
    servers[[i]] <- nsock
    active[[i]] <- ncv
    queue[[i]] <- create_req(.context(sock), cv)
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
        send(sockc, data, mode = 2L)
        cmessage <- recv_aio(sockc, mode = 5L, cv = cv)
        next
      }

      for (i in seq_n)
        if (length(queue[[i]]) > 2L && !unresolved(queue[[i]][["req"]])) {
          req <- .subset2(queue[[i]][["req"]], "value")
          if (is.object(req)) req <- serialize(req, NULL, xdr = FALSE)
          send(queue[[i]][["ctx"]], req, mode = 2L, block = TRUE)
          q <- queue[[i]][["daemon"]]
          if (req[4L]) {
            send(queue[[i]][["rctx"]], NULL, mode = 2L, block = TRUE)
            reap(queue[[i]][["rctx"]])
          } else {
            serverfree[q] <- TRUE
          }
          complete[q] <- complete[q] + 1L
          queue[[i]] <- create_req(.context(sock), cv)
        }

      free <- which(serverfree & activevec)

      if (length(free))
        for (q in free)
          for (i in seq_n) {
            if (length(queue[[i]]) == 2L && !unresolved(queue[[i]][["req"]])) {
              queue[[i]][["rctx"]] <- .context(servers[[q]])
              queue[[i]][["req"]] <- request(queue[[i]][["rctx"]], .subset2(queue[[i]][["req"]], "value"),
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
#' [DEPRECATED] This is a legacy function used only with the legacy v1
#' dispatcher, and will be removed in a future release.
#'
#' @inheritParams mirai
#' @param i integer index number URL to regenerate at dispatcher.
#' @param force [default FALSE] logical value whether to regenerate the URL even
#'   when there is an existing active connection.
#'
#' @return The regenerated character URL upon success, or else NULL.
#'
#' @keywords internal
#' @export
#'
saisei <- function(i, force = FALSE, .compute = "default") {

  envir <- ..[[.compute]]
  length(envir[["msgid"]]) && return()
  i <- as.integer(i[1L])
  length(envir[["sockc"]]) && i > 0L && i <= envir[["n"]] && !startsWith(envir[["urls"]][i], "t") || return()
  r <- query_dispatcher(envir[["sockc"]], if (force) -i else i, recv_mode = 9L)
  is.character(r) && nzchar(r) || return()
  envir[["urls"]][i] <- r
  r

}
