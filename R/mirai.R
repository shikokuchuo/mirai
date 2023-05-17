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

# mirai ------------------------------------------------------------------------

#' mirai Server (Async Executor Daemon)
#'
#' Implements a persistent executor/server for the remote process. Awaits data,
#'     evaluates an expression in an environment containing the supplied data,
#'     and returns the result to the caller/client.
#'
#' @param url the client or dispatcher URL to dial into as a character string,
#'     including the port to connect to and (optionally) a path for websocket
#'     URLs e.g. 'tcp://192.168.0.2:5555' or 'ws://192.168.0.2:5555/path'.
#' @param asyncdial [default FALSE] whether to perform dials asynchronously. The
#'     default FALSE will error if a connection is not immediately possible
#'     (e.g. \code{\link{daemons}} has yet to be called on the client, or the
#'     specified port is not open etc.). Specifying TRUE continues retrying
#'     (indefinitely) if not immediately successful, which is more resilient but
#'     can mask potential connection issues.
#' @param maxtasks [default Inf] the maximum number of tasks to execute (task
#'     limit) before exiting.
#' @param idletime [default Inf] maximum idle time, since completion of the last
#'     task (in milliseconds) before exiting.
#' @param walltime [default Inf] soft walltime, or the minimum amount of real
#'     time taken (in milliseconds) before exiting.
#' @param timerstart [default 0L] number of completed tasks after which to start
#'     the timer for 'idletime' and 'walltime'. 0L implies timers are started
#'     upon launch.
#' @param exitlinger [default 1000L] time in milliseconds to linger before
#'     exiting due to a timer / task limit, to allow sockets to complete sends
#'     currently in progress. The default can be set wider if computations are
#'     expected to return very large objects (> GBs).
#' @param ... reserved but not currently used.
#' @param cleanup [default 7L] Integer additive bitmask controlling whether to
#'     perform cleanup of the global environment (1L), reset loaded packages to
#'     an initial state (2L), reset options to an initial state (4L), and
#'     perform garbage collection (8L) after each evaluation. This option should
#'     not normally be modified. Do not set unless you are certain you require
#'     persistence across evaluations. Note: it may be an error to reset options
#'     but not loaded packages if packages set options on load.
#'
#' @return Invisible NULL.
#'
#' @details The network topology is such that server daemons dial into the
#'     client or dispatcher, which listens at the 'url' address. In this way,
#'     network resources may be added or removed dynamically and the client or
#'     dispatcher automatically distributes tasks to all available servers.
#'
#' @export
#'
server <- function(url, asyncdial = FALSE, maxtasks = Inf, idletime = Inf,
                   walltime = Inf, timerstart = 0L, exitlinger = 1000L, ...,
                   cleanup = 7L) {

  sock <- socket(protocol = "rep")
  on.exit(close(sock))
  cv <- cv()
  pipe_notify(sock, cv = cv, add = FALSE, remove = TRUE, flag = TRUE)
  dial_and_sync_socket(sock = sock, url = url, asyncdial = asyncdial)
  op <- options()
  se <- search()
  count <- 0L
  if (idletime > walltime) idletime <- walltime else if (idletime == Inf) idletime <- NULL

  devnull <- file(nullfile(), open = "w", blocking = FALSE)
  sink(file = devnull)
  sink(file = devnull, type = "message")
  on.exit({
    sink()
    sink(type = "message")
    close(devnull)
  }, add = TRUE)
  start <- mclock()
  while (count < maxtasks && mclock() - start < walltime) {

    ctx <- .context(sock)
    aio <- recv_aio_signal(ctx, mode = 1L, timeout = idletime, cv = cv)
    wait(cv) || return(invisible())
    ._mirai_. <- .subset2(call_aio(aio), "data")
    is.integer(._mirai_.) && {
      count < timerstart && {
        start <- mclock()
        next
      }
      break
    }
    data <- tryCatch(eval(expr = ._mirai_.[[".expr"]], envir = ._mirai_., enclos = NULL),
                     error = mk_mirai_error, interrupt = mk_interrupt_error)
    send(ctx, data = data, mode = 1L)
    perform_cleanup(cleanup = cleanup, op = op, se = se)
    if (count < timerstart) start <- mclock()
    count <- count + 1L

  }

  msleep(exitlinger)

}

#' mirai dot Server (Async Executor)
#'
#' Implements an ephemeral executor/server for the remote process.
#'
#' @inheritParams server
#' @param exitlinger [default 2000L] time in milliseconds to linger before
#'     exiting to allow the socket to complete sends currently in progress.
#'
#' @return Invisible NULL.
#'
#' @keywords internal
#' @export
#'
.server <- function(url, exitlinger = 2000L) {

  sock <- socket(protocol = "rep", dial = url, autostart = NA)
  on.exit(close(sock))
  ctx <- .context(sock)
  ._mirai_. <- recv(ctx, mode = 1L)
  data <- tryCatch(eval(expr = ._mirai_.[[".expr"]], envir = ._mirai_., enclos = NULL),
                   error = mk_mirai_error, interrupt = mk_interrupt_error)
  send(ctx, data = data, mode = 1L)
  msleep(exitlinger)

}

#' mirai Dispatcher
#'
#' Implements a dispatcher for tasks from a client to multiple servers for
#'     processing, using a FIFO scheduling rule, queuing tasks as required.
#'
#' @inheritParams server
#' @param client the client URL to dial as a character string (where tasks are
#'     sent from), including the port to connect to and (optionally) a path for
#'     websocket URLs e.g. 'tcp://192.168.0.2:5555' or 'ws://192.168.0.2:5555/path'.
#' @param url (optional) the URL or range of URLs the dispatcher should
#'     listen at as a character vector, including the port to connect to and
#'     (optionally) a path for websocket URLs e.g. 'tcp://192.168.0.2:5555' or
#'     'ws://192.168.0.2:5555/path'. Tasks are sent to servers dialled into
#'     these URLs. If not supplied, 'n' URLs accessible from the same computer
#'     will be assigned automatically.
#' @param n (optional) if specified, the integer number of servers to listen for.
#'     Otherwise 'n' will be inferred from the number of URLs supplied as '...'.
#'     Where a single URL is supplied and 'n' > 1, 'n' unique URLs will be
#'     automatically assigned for servers to dial into.
#' @param token [default FALSE] if TRUE, appends a unique 40-character token
#'     to each URL path the dispatcher listens at (not applicable for TCP URLs
#'     which do not accept a path).
#' @param lock [default FALSE] if TRUE, sockets lock once a connection has been
#'     accepted, preventing further connection attempts. This provides safety
#'     against more than one server trying to connect to a unique URL.
#' @param ... additional arguments passed through to \code{\link{server}} if
#'     launching local daemons i.e. 'url' is not specified.
#' @param monitor (for package internal use only) do not set this parameter.
#'
#' @return Invisible NULL.
#'
#' @details The network topology is such that a dispatcher acts as a gateway
#'     between clients and servers, ensuring that tasks received from clients
#'     are dispatched on a FIFO basis to servers for processing. Tasks are
#'     queued at the dispatcher to ensure tasks are only sent to servers that
#'     can begin immediate execution of the task.
#'
#' @export
#'
dispatcher <- function(client, url = NULL, n = NULL, asyncdial = FALSE,
                       token = FALSE, lock = FALSE, ..., monitor = NULL) {

  n <- if (is.numeric(n)) as.integer(n) else length(url)
  n > 0L || stop(.messages[["missing_url"]])

  sock <- socket(protocol = "rep")
  on.exit(close(sock))
  cv <- cv()
  pipe_notify(sock, cv = cv, add = FALSE, remove = TRUE, flag = TRUE)
  dial_and_sync_socket(sock = sock, url = client, asyncdial = asyncdial)

  auto <- is.null(url)
  vectorised <- length(url) == n
  seq_n <- seq_len(n)
  basenames <- servernames <- character(n)
  instance <- istore <- complete <- assigned <- integer(n)
  serverfree <- !integer(n)
  active <- servers <- queue <- vector(mode = "list", length = n)
  if (!auto) {
    baseurl <- parse_url(url)
    if (substr(baseurl[["scheme"]], 1L, 3L) == "tcp") {
      ports <- if (baseurl[["port"]] == "0") integer(n) else seq.int(baseurl[["port"]], length.out = n)
      token <- FALSE
    } else {
      ports <- NULL
    }
  }

  for (i in seq_n) {
    burl <- if (auto) sprintf(.urlfmt, "") else
      if (vectorised) url[i] else
        if (is.null(ports)) sprintf("%s/%d", url, i) else
          sub(ports[1L], ports[i], url, fixed = TRUE)
    basenames[i] <- burl
    nurl <- if (auto || token) new_tokenized_url(url = burl, auto = auto) else burl
    nsock <- req_socket(NULL)
    ncv <- cv()
    pipe_notify(nsock, cv = ncv, cv2 = cv, flag = FALSE)
    listen(nsock, url = nurl, error = TRUE)
    if (lock)
      lock(nsock, cv = ncv)
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

    if (auto)
      launch_daemon(type = 2L, nurl, parse_dots(...))

    servers[[i]] <- nsock
    active[[i]] <- ncv
  }

  on.exit(lapply(servers, close), add = TRUE, after = TRUE)

  ctrchannel <- is.character(monitor)
  if (ctrchannel) {
    sockc <- bus_socket(NULL)
    on.exit(close(sockc), add = TRUE, after = FALSE)
    ccv <- cv()
    dial_and_sync_socket(sock = sockc, url = monitor, asyncdial = asyncdial)
    send(sockc, c(Sys.getpid(), servernames), mode = 2L)
    csignal <- recv_aio_signal(sockc, mode = 5L, cv = ccv)
    cmessage <- recv_aio_signal(sockc, mode = 5L, cv = cv)
  }

  suspendInterrupts(
    repeat {

      wait(cv) || break

      cv_values <- as.integer(lapply(active, cv_value))
      activevec <- cv_values %% 2L
      instance <- (cv_values + activevec) / 2L
      changes <- (instance - istore) > 0L
      istore <- instance
      if (any(changes)) {
        assigned[changes] <- 0L
        complete[changes] <- 0L
      }

      ctrchannel && cv_value(ccv) && {
        wait(ccv)
        i <- .subset2(call_aio(cmessage), "data")
        if (i) {
          if ((i > 0L && i <= n && !activevec[i] || i < 0L && (i <- -i) <= n) &&
              substr(basenames[i], 1L, 3L) != "tcp") {
            close(attr(servers[[i]], "listener")[[1L]])
            attr(servers[[i]], "listener") <- NULL
            data <- servernames[i] <- new_tokenized_url(url = basenames[i], auto = auto)
            cv_reset(active[[i]])
            listen(servers[[i]], url = data, error = TRUE)
          } else {
            data <- ""
          }
        } else {
          data <- as.integer(c(activevec, instance, assigned, complete))
        }
        send(sockc, data = data, mode = 2L)
        csignal <- recv_aio_signal(sockc, mode = 5L, cv = ccv)
        cmessage <- recv_aio_signal(sockc, mode = 5L, cv = cv)
        next
      }

      free <- which(serverfree & activevec)

      if (length(free))
        for (q in free)
          for (i in seq_n) {
            if (length(queue[[i]]) == 2L && !unresolved(queue[[i]][["req"]])) {
              queue[[i]][["res"]] <- request_signal(.context(servers[[q]]), data = .subset2(queue[[i]][["req"]], "data"), send_mode = 1L, recv_mode = 1L, cv = cv)
              queue[[i]][["daemon"]] <- q
              serverfree[q] <- FALSE
              assigned[q] <- assigned[q] + 1L
              break
            }
            serverfree[q] || break
          }

      for (i in seq_n)
        if (length(queue[[i]]) > 2L && !unresolved(queue[[i]][["res"]])) {
          send(queue[[i]][["ctx"]], data = .subset2(queue[[i]][["res"]], "data"), mode = 1L)
          q <- queue[[i]][["daemon"]]
          serverfree[q] <- TRUE
          complete[q] <- complete[q] + 1L
          queue[[i]] <- list()
        }

      for (i in which(activevec == 1L))
        if (!length(queue[[i]])) {
          ctx <- context(sock)
          req <- recv_aio_signal(ctx, mode = 1L, cv = cv)
          queue[[i]] <- list(ctx = ctx, req = req)
        }

      for (i in which(activevec == 0L))
        if (length(queue[[i]]) == 2L && unresolved(queue[[i]][["req"]])) {
          close(queue[[i]][["ctx"]])
          queue[[i]] <- list()
        }

    }
  )

}

#' mirai (Evaluate Async)
#'
#' Evaluate an expression asynchronously in a new background R process or
#'     persistent daemon (local or remote). This function will return
#'     immediately with a 'mirai', which will resolve to the evaluated result
#'     once complete.
#'
#' @param .expr an expression to evaluate asynchronously (of arbitrary length,
#'     wrapped in \{\} if necessary), or a language object passed by \link{name}.
#' @param ... (optional) named arguments (name = value pairs) specifying
#'     objects referenced in '.expr'. Used in addition to and taking precedence
#'     over, any arguments specified via '.args'.
#' @param .args (optional) either (i) a list of objects to be passed by
#'     \link{name}, i.e. also found in the current scope with the same name, or
#'     else (ii) a list of name = value pairs, as in '...'.
#' @param .timeout [default NULL] for no timeout, or an integer value in
#'     milliseconds. A mirai will resolve to an 'errorValue' 5 (timed out) if
#'     evaluation exceeds this limit.
#' @param .compute [default 'default'] character value for the compute profile
#'     to use when sending the mirai.
#'
#' @return A 'mirai' object.
#'
#' @details This function will return a 'mirai' object immediately.
#'
#'     The value of a mirai may be accessed at any time at \code{$data}, and
#'     if yet to resolve, an 'unresolved' logical NA will be returned instead.
#'
#'     \code{\link{unresolved}} may be used on a mirai, returning TRUE if a
#'     'mirai' has yet to resolve and FALSE otherwise. This is suitable for use
#'     in control flow statements such as \code{while} or \code{if}.
#'
#'     Alternatively, to call (and wait for) the result, use \code{\link{call_mirai}}
#'     on the returned mirai. This will block until the result is returned
#'     (although interruptible with e.g. ctrl+c).
#'
#'     The expression '.expr' will be evaluated in a separate R process in a
#'     clean environment, which is not the global environment, consisting only
#'     of the named objects passed as '...' and/or the list supplied to '.args'.
#'
#'     If an error occurs in evaluation, the error message is returned as a
#'     character string of class 'miraiError' and 'errorValue'.
#'     \code{\link{is_mirai_error}} may be used to test for this.
#'
#'     \code{\link{is_error_value}} tests for all error conditions including
#'     'mirai' errors, interrupts, and timeouts.
#'
#'     Specify '.compute' to send the mirai using a specific compute profile (if
#'     previously created by \code{\link{daemons}}), otherwise leave as 'default'.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' # specifying objects via '...'
#' n <- 3
#' m <- mirai(x + y + 2, x = 2, y = n)
#' m
#' m$data
#' Sys.sleep(0.2)
#' m$data
#'
#' # passing existing objects by name via '.args'
#' df1 <- data.frame(a = 1, b = 2)
#' df2 <- data.frame(a = 3, b = 1)
#' m <- mirai(as.matrix(rbind(df1, df2)), .args = list(df1, df2), .timeout = 1000)
#' call_mirai(m)$data
#'
#' # using unresolved()
#' m <- mirai(
#'   {
#'     res <- rnorm(n)
#'     res / rev(res)
#'   },
#'   n = 1e6
#' )
#' while (unresolved(m)) {
#'   cat("unresolved\n")
#'   Sys.sleep(0.1)
#' }
#' str(m$data)
#'
#' # evaluating scripts using source(local = TRUE) in '.expr'
#' n <- 10L
#' file <- tempfile()
#' cat("r <- rnorm(n)", file = file)
#' m <- mirai({source(file, local = TRUE); r}, .args = list(file, n))
#' call_mirai(m)[["data"]]
#' unlink(file)
#'
#' # specifying global variables using list2env(envir = .GlobalEnv) in '.expr'
#' n <- 10L
#' file <- tempfile()
#' cat("r <- rnorm(n)", file = file)
#' globals <- list(file = file, n = n)
#' m <- mirai(
#'   {
#'     list2env(globals, envir = .GlobalEnv)
#'     source(file)
#'     r
#'   },
#'   globals = globals
#' )
#' call_mirai(m)[["data"]]
#' unlink(file)
#'
#' # passing a language object to '.expr' and a named list to '.args'
#' expr <- quote(a + b + 2)
#' args <- list(a = 2, b = 3)
#' m <- mirai(.expr = expr, .args = args)
#' call_mirai(m)$data
#'
#' }
#'
#' @export
#'
mirai <- function(.expr, ..., .args = list(), .timeout = NULL, .compute = "default") {

  missing(.expr) && stop(.messages[["missing_expression"]])

  expr <- substitute(.expr)
  arglist <- list(..., .expr = if (is.symbol(expr) && is.language(get0(expr, envir = sys.frame(-1L)))) .expr else expr)

  if (length(.args)) {
    is.list(.args) || stop(.messages[["requires_list"]])
    arglist <- if (length(names(.args))) c(.args, arglist) else
      c(`names<-`(.args, `storage.mode<-`(substitute(.args)[-1L], "character")), arglist)
  }

  envir <- list2env(arglist, envir = NULL, parent = .GlobalEnv)

  if (length(..[[.compute]][["sock"]])) {
    aio <- request(.context(..[[.compute]][["sock"]]), data = envir, send_mode = 1L, recv_mode = 1L, timeout = .timeout)

  } else {
    url <- auto_tokenized_url()
    sock <- req_socket(url)
    if (length(.timeout)) launch_and_sync_daemon(sock = sock, type = 1L, url) else launch_daemon(type = 1L, url)
    aio <- request(.context(sock), data = envir, send_mode = 1L, recv_mode = 1L, timeout = .timeout)
    `attr<-`(.subset2(aio, "aio"), "sock", sock)

  }

  `class<-`(aio, c("mirai", "recvAio"))

}

#' daemons (Persistent Server Processes)
#'
#' Set 'daemons' or background persistent server processes receiving
#'     \code{\link{mirai}} requests. These are, by default, automatically
#'     created on the local machine. Alternatively, a client URL may be
#'     specified to receive connections from remote servers started with
#'     \code{\link{server}} for distributing tasks across the network. Daemons
#'     may take advantage of the dispatcher, which ensures that tasks are
#'     assigned to servers efficiently on a FIFO basis, or else the low-level
#'     approach of distributing tasks to servers in an even fashion.
#'
#' @param n integer number of daemons (server processes) to set.
#' @param url [default NULL] if specified (for connecting to remote servers),
#'     the client URL as a character vector, including a port accepting incoming
#'     connections (and optionally for websockets a path) e.g.
#'     'tcp://192.168.0.2:5555' or 'ws://192.168.0.2:5555/path'.
#' @param dispatcher [default TRUE] logical value whether to use dispatcher.
#'     Dispatcher is a background process that connects to servers on behalf of
#'     the client and ensures FIFO scheduling, queueing tasks if necessary
#'     (see Dispatcher section below).
#' @param ... additional arguments passed through to \code{\link{dispatcher}} if
#'     using dispatcher and/or \code{\link{server}} if launching local daemons.
#' @param .compute [default 'default'] character compute profile to use for
#'     creating the daemons (each compute profile has its own set of daemons for
#'     connecting to different resources).
#'
#' @return Setting daemons: integer number of daemons set, or the character
#'     client URL.
#'
#'     Viewing current status: a named list comprising: \itemize{
#'     \item{\strong{connections}} {- number of active connections at the client.
#'     Always 1L when using dispatcher as there is only a single connection to
#'     the dispatcher, which then in turn connects to the servers.}
#'     \item{\strong{daemons}} {- if using dispatcher: a status matrix (see
#'     Status Matrix section below), or else an integer 'errorValue' if
#'     communication with the dispatcher was unsuccessful. If not using
#'     dispatcher: the number of daemons set, or else the client URL.}
#'     }
#'
#' @details For viewing the currrent status, specify \code{daemons()} with no
#'     arguments.
#'
#'     Use \code{daemons(0)} to reset daemon connections:
#'     \itemize{
#'     \item{A reset is required before revising settings for the same compute
#'     profile, otherwise changes are not registered.}
#'     \item{All connected daemons and/or dispatchers exit automatically.}
#'     \item{\{mirai\} reverts to the default behaviour of creating a new
#'     background process for each request.}
#'     }
#'
#'     When specifying a client URL, all daemons dialing into the client are
#'     detected automatically and resources may be added or removed at any time.
#'
#'     If the client session ends, for whatever reason, all connected dispatcher
#'     and daemon processes automatically exit as soon as their connections are
#'     dropped. If a daemon is processing a task, it will exit as soon as the
#'     task is complete.
#'
#' @section Dispatcher:
#'
#'     By default \code{dispatcher = TRUE}. This launches a background process
#'     running \code{\link{dispatcher}}.  A dispatcher connects to servers on
#'     behalf of the client and queues tasks until a server is able to begin
#'     immediate execution of that task, ensuring FIFO scheduling. Dispatcher
#'     uses synchronisation primitives from \code{nanonext}, waiting rather than
#'     polling for tasks, which is efficient both in terms of consuming no
#'     resources while waiting, and also being fully synchronised with events
#'     (having no latency).
#'
#'     By specifying \code{dispatcher = FALSE}, servers connect to the client
#'     directly rather than through a dispatcher. The client sends tasks to
#'     connected servers immediately in an evenly-distributed fashion. However,
#'     optimal scheduling is not guaranteed as the duration of tasks cannot be
#'     known \emph{a priori}, such that tasks can be queued at a server behind
#'     a long-running task while other servers remain idle. Nevertheless, this
#'     provides a resource-light approach suited to working with similar-length
#'     tasks, or where concurrent tasks typically do not exceed available daemons.
#'
#' @section Local Daemons:
#'
#'     Daemons provide a potentially more efficient solution for asynchronous
#'     operations as new processes no longer need to be created on an \emph{ad
#'     hoc} basis.
#'
#'     Supply the argument 'n' to set the number of daemons. New background
#'     \code{\link{server}} processes are automatically created on the local
#'     machine connecting back to the client process, either directly or via a
#'     dispatcher.
#'
#' @section Distributed Computing:
#'
#'     Specifying 'url' allows tasks to be distributed across the network.
#'
#'     The client URL should be in the form of a character string such as:
#'     'tcp://192.168.0.2:5555' at which server processes started using
#'     \code{\link{server}} should connect to.
#'
#'     Alternatively, to listen to port 5555 on all interfaces on the local host,
#'     specify either 'tcp://:5555', 'tcp://*:5555' or 'tcp://0.0.0.0:5555'.
#'
#'     Specifying the wildcard value zero for the port number e.g. 'tcp://:0' or
#'     'ws://:0' will automatically assign a free ephemeral port. Use
#'     \code{daemons()} to inspect the actual assigned port at any time.
#'
#'     \strong{With Dispatcher}
#'
#'     When using dispatcher, it is recommended to use a websocket URL rather
#'     than TCP, as this requires only one port to connect to all servers: a
#'     websocket URL supports a path after the port number, which can be made
#'     unique for each server.
#'
#'     Specifying a single client URL such as 'ws://192.168.0.2:5555' with
#'     \code{n = 6} will automatically append a sequence to the path, listening
#'     to the URLs 'ws://192.168.0.2:5555/1' through 'ws://192.168.0.2:5555/6'.
#'
#'     Alternatively, specify a vector of URLs to listen to arbitrary port
#'     numbers / paths. In this case it is optional to supply 'n' as this can
#'     be inferred by the length of vector supplied.
#'
#'     Individual \code{\link{server}} instances should then be started on the
#'     remote resource, which dial in to each of these client URLs. At most one
#'     server should be dialled into each URL at any given time.
#'
#'     The dispatcher automatically adjusts to the number of servers actually
#'     connected. Hence it is possible to dynamically scale up or down the
#'     number of servers as required, subject to the maximum number initially
#'     specified.
#'
#'     Alternatively, supplying a single TCP URL will listen on a block of URLs
#'     with ports starting from the supplied port number and incrementing by one
#'     for 'n' specified e.g. the client URL 'tcp://192.168.0.2:5555' with
#'     \code{n = 6} listens to the contiguous block of ports 5555 through 5560.
#'
#'     \strong{Without Dispatcher}
#'
#'     A TCP URL may be used in this case as the client listens at only one
#'     address, utilising a single port.
#'
#'     The network topology is such that server daemons (started with
#'     \code{\link{server}}) or indeed dispatchers (started with
#'     \code{\link{dispatcher}}) dial into the same client URL.
#'
#'     'n' is not required in this case, and disregarded if supplied, as network
#'     resources may be added or removed at any time. The client automatically
#'     distributes tasks to all connected servers and dispatchers.
#'
#' @section Compute Profiles:
#'
#'     By default, the 'default' compute profile is used. Providing a character
#'     value for '.compute' creates a new compute profile with the name
#'     specified. Each compute profile retains its own daemons settings, and may
#'     be operated independently of each other. Some usage examples follow:
#'
#'     \strong{local / remote} daemons may be set via a client URL and creating
#'     a new compute profile by specifying '.compute' as 'remote'. Subsequent
#'     mirai calls may then be sent for local computation by not specifying its
#'     '.compute' argument, or for remote computation to connected daemons by
#'     specifying its '.compute' argument as 'remote'.
#'
#'     \strong{cpu / gpu} some tasks may require access to different classes of
#'     server, such as those with GPUs. In this case, \code{daemons()} may be
#'     called twice to set up client URLs for CPU-only and GPU servers to dial
#'     into, specifying the '.compute' argument as 'cpu' and 'gpu' respectively.
#'     By supplying the '.compute' argument to subsequent mirai calls, tasks may
#'     be sent to either 'cpu' or 'gpu' servers as appropriate.
#'
#'     Note: further actions such as viewing the status of daemons or resetting
#'     via \code{daemons(0)} should be carried out with the desired '.compute'
#'     argument specified.
#'
#' @section Status Matrix:
#'
#'     When using dispatcher, calling \code{daemons()} returns a matrix with the
#'     following columns:
#'
#'     'online' shows as 1 when there is an active connection, or else 0 if a
#'     server has yet to connect or has disconnected.
#'
#'     'instance' increments by 1 every time there is a new connection at a URL.
#'     When this happens, the 'assigned' and 'complete' statistics reset to zero.
#'     This counter is designed to track new server instances connecting after
#'     previous ones have ended (due to time-outs etc.). 'instance' itself
#'     resets to zero if the URL is regenerated by \code{\link{saisei}}.
#'
#'     'assigned' shows the cumulative number of tasks assigned to the server
#'     instance by the dispatcher.
#'
#'     'complete' shows the cumulative number of tasks completed by the server
#'     instance.
#'
#'     The URLs are stored as row names to the matrix.
#'
#' @section Timeouts:
#'
#'     Specifying the \code{.timeout} argument in \code{\link{mirai}} will ensure
#'     that the 'mirai' always resolves.
#'
#'     However, the task may not have completed and still be ongoing in the
#'     daemon process. In such situations, dispatcher ensures that queued tasks
#'     are not assigned to the busy process, however overall performance may
#'     still be degraded if they remain in use. If a process hangs and cannot be
#'     restarted manually, \code{\link{saisei}} specifying \code{force = TRUE}
#'     may be used to regenerate any particular URL for a new \code{\link{server}}
#'     to connect to.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' # Create 2 local daemons (using dispatcher)
#' daemons(2)
#' # View status
#' daemons()
#' # Reset to zero
#' daemons(0)
#'
#' # Create 2 local daemons (not using dispatcher)
#' daemons(2, dispatcher = FALSE)
#' # View status
#' daemons()
#' # Reset to zero
#' daemons(0)
#'
#' # 2 remote daemons via dispatcher (using zero wildcard)
#' daemons(2, url = "ws://:0")
#' # View status
#' daemons()
#' # Reset to zero
#' daemons(0)
#'
#' # Set client URL for remote servers to dial into (using zero wildcard)
#' daemons(url = "tcp://:0", dispatcher = FALSE)
#' # View status
#' daemons()
#' # Reset to zero
#' daemons(0)
#'
#' }
#'
#' @export
#'
daemons <- function(n, url = NULL, dispatcher = TRUE, ..., .compute = "default") {

  envir <- ..[[.compute]]
  missing(n) && missing(url) &&
    return(list(connections = if (length(envir[["sock"]])) stat(envir[["sock"]], "pipes") else 0L,
                daemons = if (length(envir[["sockc"]])) query_status(envir) else envir[["proc"]] %||% 0L))

  if (is.null(envir)) {
    `[[<-`(.., .compute, new.env(hash = FALSE, parent = environment(daemons)))
    envir <- ..[[.compute]]
  }

  if (is.character(url)) {

    if (is.null(envir[["sock"]])) {
      if (dispatcher) {
        n <- if (missing(n)) length(url) else if (is.numeric(n) && n > 0L) as.integer(n) else stop(.messages[["n_one"]])
        parse_url(url)
        urld <- auto_tokenized_url()
        urlc <- new_control_url(urld)
        sock <- req_socket(urld)
        sockc <- bus_socket(urlc)
        launch_and_sync_daemon(sock = sock, type = 5L, urld, url, n, urlc, parse_dots(...))
        recv_and_store(sockc = sockc, envir = envir)
        proc <- n
      } else {
        sock <- req_socket(url)
        listener <- attr(sock, "listener")[[1L]]
        proc <- opt(listener, "url")
        if (parse_url(proc)[["port"]] == "0")
          proc <- sub_real_port(port = opt(listener, "tcp-bound-port"), url = proc)
      }
      `[[<-`(`[[<-`(envir, "sock", sock), "proc", proc)
    }

  } else {

    is.numeric(n) || stop(.messages[["numeric_n"]])
    n <- as.integer(n)

    if (n == 0L) {
      length(envir[["proc"]]) || return(0L)

      close(envir[["sock"]])
      if (length(envir[["sockc"]])) {
        close(envir[["sockc"]])
        `[[<-`(envir, "sockc", NULL)
      }
      `[[<-`(`[[<-`(envir, "sock", NULL), "proc", NULL)

    } else if (is.null(envir[["sock"]])) {

      n > 0L || stop(.messages[["n_zero"]])
      urld <- auto_tokenized_url()
      sock <- req_socket(urld)
      if (dispatcher) {
        urlc <- new_control_url(urld)
        sockc <- bus_socket(urlc)
        launch_and_sync_daemon(sock = sock, type = 4L, urld, n, urlc, parse_dots(...))
        recv_and_store(sockc = sockc, envir = envir)
      } else {
        for (i in seq_len(n))
          launch_daemon(type = 2L, urld, parse_dots(...))
      }
      `[[<-`(`[[<-`(envir, "sock", sock), "proc", n)
    }

  }

  envir[["proc"]] %||% 0L

}

#' Launch mirai Server
#'
#' Utility function which calls \code{\link{server}} in a background
#'     \code{Rscript} process. May be used to re-launch local daemons that have
#'     timed out.
#'
#' @param url the client URL for the server to dial into as a character string,
#'     including the port to connect to and (optionally) a path for websocket
#'     URLs e.g. tcp://192.168.0.2:5555' or 'ws://192.168.0.2:5555/path'.
#' @param ... (optional) additional arguments passed to \code{\link{server}}.
#'
#' @return Invisibly, integer system exit code (zero upon success).
#'
#' @details Consider specifying the argument 'asyncdial' [default FALSE] whether
#'     to perform dials asynchronously. The default FALSE will error if a
#'     connection is not immediately possible (e.g. \code{\link{daemons}} has
#'     yet to be called on the client, or the specified port is not open etc.).
#'     Specifying TRUE continues retrying (indefinitely) if not immediately
#'     successful, which is more resilient but can mask potential connection
#'     issues.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' launch_server("abstract://mirai", asyncdial = FALSE, idletime = 60000L)
#'
#' }
#'
#' @export
#'
launch_server <- function(url, ...) {

  parse_url(url)
  launch_daemon(type = 2L, url, parse_dots(...))

}

#' Saisei - Regenerate Token
#'
#' When using daemons with a local dispatcher service, regenerates the token for
#'     the URL a dispatcher socket listens at.
#'
#' @param i [default 1L] integer \code{i}th URL to replace.
#' @param force [default FALSE] logical value whether to replace the listener
#'     even when there is an existing connection.
#' @param .compute [default 'default'] character compute profile to use (each
#'     compute profile has its own set of daemons for connecting to different
#'     resources).
#'
#' @return The regenerated character URL upon success, or else NULL.
#'
#' @details As the specified listener is closed and replaced immediately, this
#'     function will only be successful if there are no existing connections at
#'     the socket (i.e. 'online' status shows 0), unless the argument 'force' is
#'     specified as TRUE.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' daemons(1L)
#' Sys.sleep(1L)
#' daemons()
#' saisei(i = 1L, force = TRUE)
#' daemons()
#'
#' daemons(0)
#'
#' }
#'
#' @export
#'
saisei <- function(i = 1L, force = FALSE, .compute = "default") {

  envir <- ..[[.compute]]
  length(envir[["sockc"]]) || return()
  r <- query_dispatcher(sock = envir[["sockc"]], command = as.integer(if (force) -i else i), mode = 2L)
  is.character(r) && nzchar(r) || return()
  envir[["urls"]][i] <- r
  r

}

#' mirai (Call Value)
#'
#' Call the value of a mirai, waiting for the the asynchronous operation to
#'     resolve if it is still in progress.
#'
#' @param aio a 'mirai' object.
#'
#' @return The passed mirai (invisibly). The retrieved value is stored at \code{$data}.
#'
#' @details This function will wait for the async operation to complete if still
#'     in progress (blocking).
#'
#'     A blocking call can be sent a user interrupt with e.g. ctrl+c. If the
#'     ongoing execution in the mirai is interruptible, it will resolve into
#'     an object of class 'miraiInterrupt' and 'errorValue'.
#'     \code{\link{is_mirai_interrupt}} may be used to handle such cases.
#'
#'     If an error occurs in evaluation, the error message is returned as a
#'     character string of class 'miraiError' and 'errorValue'.
#'     \code{\link{is_mirai_error}} may be used to test for this.
#'
#'     \code{\link{is_error_value}} tests for all error conditions including
#'     mirai errors, interrupts, and timeouts.
#'
#'     The mirai updates itself in place, so to access the value of a mirai
#'     \code{x} directly, use \code{call_mirai(x)$data}.
#'
#' @section Alternatively:
#'
#'     The value of a mirai may be accessed at any time at \code{$data}, and
#'     if yet to resolve, an 'unresolved' logical NA will be returned instead.
#'
#'     Using \code{\link{unresolved}} on a mirai returns TRUE only if a mirai
#'     has yet to resolve and FALSE otherwise. This is suitable for use in
#'     control flow statements such as \code{while} or \code{if}.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' # using call_mirai()
#' df1 <- data.frame(a = 1, b = 2)
#' df2 <- data.frame(a = 3, b = 1)
#' m <- mirai(as.matrix(rbind(df1, df2)), .args = list(df1, df2), .timeout = 1000)
#' call_mirai(m)$data
#'
#' # using unresolved()
#' m <- mirai({
#'   res <- rnorm(n)
#'   res / rev(res)
#'   },
#'   n = 1e6)
#' while (unresolved(m)) {
#'   cat("unresolved\n")
#'   Sys.sleep(0.1)
#' }
#' str(m$data)
#'
#' }
#'
#' @export
#'
call_mirai <- call_aio

#' mirai (Stop Evaluation)
#'
#' Stop evaluation of a mirai that is in progress.
#'
#' @param aio a 'mirai' object.
#'
#' @return Invisible NULL.
#'
#' @details Stops the asynchronous operation associated with the mirai by
#'     aborting, and then waits for it to complete or to be completely aborted.
#'     The mirai is then deallocated and attempting to access the value at
#'     \code{$data} will result in an error.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' s <- mirai(Sys.sleep(n), n = 5)
#' stop_mirai(s)
#'
#' }
#'
#' @export
#'
stop_mirai <- stop_aio

#' Query if a mirai is Unresolved
#'
#' Query whether a mirai or mirai value remains unresolved. Unlike
#'     \code{\link{call_mirai}}, this function does not wait for completion.
#'
#' @param aio a 'mirai' object or 'mirai' value stored at \code{$data}.
#'
#' @return Logical TRUE if 'aio' is an unresolved mirai or mirai value, or
#'     FALSE otherwise.
#'
#' @details Suitable for use in control flow statements such as \code{while} or
#'     \code{if}.
#'
#'     Note: querying resolution may cause a previously unresolved 'mirai' to
#'     resolve.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- mirai(Sys.sleep(0.1))
#' unresolved(m)
#' Sys.sleep(0.3)
#' unresolved(m)
#'
#' }
#'
#' @export
#'
unresolved <- unresolved

#' Is mirai
#'
#' Is the object a 'mirai'.
#'
#' @param x an object.
#'
#' @return Logical TRUE if 'x' is of class 'mirai', FALSE otherwise.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' df <- data.frame()
#' m <- mirai(as.matrix(df), .args = list(df))
#' is_mirai(m)
#' is_mirai(df)
#'
#' }
#'
#' @export
#'
is_mirai <- function(x) inherits(x, "mirai")

#' Error Validators
#'
#' Validator functions for error value types created by \{mirai\}.
#'
#' @param x an object.
#'
#' @return Logical value TRUE or FALSE.
#'
#' @details Is the object a 'miraiError'. When execution in a mirai process fails,
#'     the error message is returned as a character string of class 'miraiError'
#'     and 'errorValue'.
#'
#'     Is the object a 'miraiInterrupt'. When a mirai is sent a user interrupt,
#'     e.g. by ctrl+c during an ongoing \code{\link{call_mirai}}, the mirai
#'     will resolve to an empty character string classed as 'miraiInterrupt' and
#'     'errorValue'.
#'
#'     Is the object an 'errorValue', such as a mirai timeout, a 'miraiError'
#'     or a 'miraiInterrupt'. This is a catch-all condition that includes all
#'     returned error values, such as timeouts, as well as the error types above.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- mirai(stop())
#' call_mirai(m)
#' is_mirai_error(m$data)
#' is_mirai_interrupt(m$data)
#' is_error_value(m$data)
#'
#' m2 <- mirai(Sys.sleep(1L), .timeout = 100)
#' call_mirai(m2)
#' is_mirai_error(m2$data)
#' is_mirai_interrupt(m2$data)
#' is_error_value(m2$data)
#'
#' }
#'
#' @export
#'
is_mirai_error <- function(x) inherits(x, "miraiError")

#' @rdname is_mirai_error
#' @export
#'
is_mirai_interrupt <- function(x) inherits(x, "miraiInterrupt")

#' @rdname is_mirai_error
#' @export
#'
is_error_value <- is_error_value

#' @export
#'
print.mirai <- function(x, ...) {

  cat("< mirai >\n - $data for evaluated result\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.miraiError <- function(x, ...) {

  cat(sprintf("'miraiError' chr %s\n", x), file = stdout())
  invisible(x)

}

#' @export
#'
print.miraiInterrupt <- function(x, ...) {

  cat("'miraiInterrupt' chr ''", file = stdout())
  invisible(x)

}

# internals --------------------------------------------------------------------

launch_daemon <- function(type, ...) {
  args <- switch(type,
                 sprintf("mirai::.server(\"%s\")", ..1),
                 sprintf("mirai::server(\"%s\"%s)", ..1, ..2),
                 sprintf("mirai::server(\"%s\"%s,asyncdial=%s)", ..1, ..2, ..3),
                 sprintf("mirai::dispatcher(\"%s\",n=%d,monitor=\"%s\"%s)", ..1, ..2, ..3, ..4),
                 sprintf("mirai::dispatcher(\"%s\",c(%s),n=%d,monitor=\"%s\"%s)",
                         ..1, paste(sprintf("\"%s\"", ..2), collapse = ","), ..3, ..4, ..5))
  system2(command = .command, args = c("-e", shQuote(args)), stdout = NULL, stderr = NULL, wait = FALSE)
}

launch_and_sync_daemon <- function(sock, type, ...) {
  cv <- cv()
  pipe_notify(sock, cv = cv, add = TRUE, remove = FALSE, flag = TRUE)
  launch_daemon(type = type, ...)
  until(cv, .timelimit) && stop(.messages[["connection_timeout"]])
}

dial_and_sync_socket <- function(sock, url, asyncdial) {
  cv <- cv()
  pipe_notify(sock, cv = cv, add = TRUE, remove = FALSE, flag = FALSE)
  dial(sock, url = url, autostart = asyncdial || NA, error = TRUE)
  wait(cv)
}

sub_real_port <- function(port, url)
  sub("(?<=:)0(?![^/])", port, url, perl = TRUE)

auto_tokenized_url <- function() sprintf(.urlfmt, sha1(random(8L)))

new_control_url <- function(url) sprintf("%s%s", url, "c")

new_tokenized_url <- function(url, auto)
  sprintf(if (auto) "%s%s" else "%s/%s", url, sha1(random(8L)))

parse_dots <- function(...)
  if (missing(...)) "" else
    sprintf(",%s", paste(names(dots <- as.expression(list(...))), dots, sep = "=", collapse = ","))

req_socket <- function(url)
  `opt<-`(socket(protocol = "req", listen = url), "req:resend-time", .Machine[["integer.max"]])

bus_socket <- function(url)
  `opt<-`(socket(protocol = "bus", listen = url), "recv-buffer", 2L)

query_dispatcher <- function(sock, command, mode) {
  send(sock, data = command, mode = 2L)
  send(sock, data = command, mode = 2L)
  recv(sock, mode = mode, block = .timelimit)
}

query_status <- function(envir) {
  res <- query_dispatcher(sock = envir[["sockc"]], command = 0L, mode = 5L)
  is_error_value(res) && return(res)
  `attributes<-`(res, list(dim = c(length(envir[["urls"]]), 4L),
                           dimnames = list(`attr<-`(envir[["urls"]], "dispatcher_pid", envir[["pid"]]),
                                           c("online", "instance", "assigned", "complete"))))
}

recv_and_store <- function(sockc, envir) {
  res <- recv(sockc, mode = 2L, block = .timelimit)
  is.integer(res) && stop(.messages[["connection_timeout"]])
  `[[<-`(`[[<-`(`[[<-`(envir, "sockc", sockc), "urls", res[-1L]), "pid", as.integer(res[1L]))
}

perform_cleanup <- function(cleanup, op, se) {
  if (cleanup > 7L) {
    gc(verbose = FALSE)
    cleanup <- cleanup - 7L
  }
  if (cleanup > 3L) {
    options(op)
    cleanup <- cleanup - 3L
  }
  if (cleanup > 1L) {
    lapply((new <- search())[!new %in% se], detach, unload = TRUE, character.only = TRUE)
    cleanup <- cleanup - 1L
  }
  if (cleanup > 0L)
    rm(list = ls(.GlobalEnv, all.names = TRUE, sorted = FALSE), envir = .GlobalEnv)
}

mk_interrupt_error <- function(e) `class<-`("", c("miraiInterrupt", "errorValue"))

mk_mirai_error <- function(e) {
  call <- deparse(.subset2(e, "call"), backtick = TRUE, control = NULL, nlines = 1L)
  msg <- if (call == "NULL" || call == "eval(expr = ._mirai_.[[\".expr\"]], envir = ._mirai_., enclos = NULL)")
    sprintf("Error: %s", .subset2(e, "message")) else
      sprintf("Error in %s: %s", call, .subset2(e, "message"))
  `class<-`(msg, c("miraiError", "errorValue"))
}

`%||%` <- function(x, y) if (length(x)) x else y
