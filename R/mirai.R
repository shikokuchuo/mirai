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
#' @param asyncdial [default TRUE] whether to perform dials asynchronously. An
#'     asynchronous dial is more resilient and will continue retrying if not
#'     immediately successful. However this can mask potential connection issues
#'     and specifying FALSE will error if a connection is not immediately
#'     possible (e.g. \code{\link{daemons}} has yet to be called on the client,
#'     or the specified port is not open etc.).
#' @param maxtasks [default Inf] the maximum number of tasks to execute (task
#'     limit) before exiting.
#' @param idletime [default Inf] maximum idle time, since completion of the last
#'     task (in milliseconds) before exiting.
#' @param walltime [default Inf] soft walltime, or the minimum amount of real
#'     time taken (in milliseconds) before exiting.
#' @param timerstart [default 0L] number of completed tasks after which to start
#'     the timer for 'idletime' and 'walltime'. 0L implies timers are started
#'     upon launch.
#' @param exitlinger [default 100L] time in milliseconds to linger after an exit
#'     signal is received or a timer / task limit is reached, to allow sockets
#'     to flush sends currently in progress. The default permits normal
#'     operations, but should be set wider if computations are expected to
#'     return very large objects.
#' @param ... reserved but not currently used.
#' @param cleanup [default TRUE] logical value whether to perform cleanup of the
#'     global environment, options values and loaded packages after each task
#'     evaluation. This option should not be modified. Do not set to FALSE
#'     unless you are certain you want such persistence across evaluations.
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
server <- function(url, asyncdial = TRUE, maxtasks = Inf, idletime = Inf,
                   walltime = Inf, timerstart = 0L, exitlinger = 100L, ...,
                   cleanup = TRUE) {

  sock <- socket(protocol = "rep", dial = url, autostart = if (asyncdial) TRUE else NA)
  cv <- cv()
  pipe_notify(sock, cv = cv, add = FALSE, remove = TRUE)

  devnull <- file(nullfile(), open = "w", blocking = FALSE)
  sink(file = devnull)
  sink(file = devnull, type = "message")
  on.exit(expr = {
    msleep(exitlinger)
    close(sock)
    sink()
    sink(type = "message")
    close(devnull)
  })
  count <- 0L
  if (idletime > walltime) idletime <- walltime else if (idletime == Inf) idletime <- NULL
  op <- options()
  se <- search()
  start <- mclock()

  while (count < maxtasks && mclock() - start < walltime) {

    ctx <- context(sock)
    aio <- recv_aio_signal(ctx, mode = 1L, timeout = idletime, cv = cv)
    wait(cv)
    .unresolved(aio) && break
    envir <- .subset2(aio, "data")
    is.integer(envir) && {
      count < timerstart && {
        start <- mclock()
        next
      }
      break
    }
    data <- tryCatch(eval(expr = envir[[".expr"]], envir = envir, enclos = NULL),
                     error = mk_mirai_error, interrupt = mk_interrupt_error)
    send(ctx, data = data, mode = 1L)
    if (cleanup) {
      rm(list = ls(.GlobalEnv, all.names = TRUE, sorted = FALSE), envir = .GlobalEnv)
      lapply((new <- search())[!new %in% se], detach, unload = TRUE, character.only = TRUE)
      options(op)
    }
    if (count < timerstart) start <- mclock()
    count <- count + 1L

  }

}

#' mirai dot Daemon
#'
#' Implements an ephemeral executor/server for the remote process.
#'
#' @inheritParams server
#'
#' @return Invisible NULL.
#'
#' @keywords internal
#' @export
#'
. <- function(url) {

  sock <- socket(protocol = "rep", dial = url)
  on.exit(expr = close(sock))
  ctx <- context(sock)
  envir <- recv(ctx, mode = 1L)
  data <- tryCatch(eval(expr = envir[[".expr"]], envir = envir, enclos = NULL),
                   error = mk_mirai_error, interrupt = mk_interrupt_error)
  send(ctx, data = data, mode = 1L)
  msleep(2000L)

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
#' @param exitlinger [default 100L] time in milliseconds to linger after an exit
#'     signal is received, to allow sockets to flush sends currently in progress.
#'     The default permits normal operations, but should be set wider if
#'     computations are expected to return very large objects.
#' @param pollfreqh [default 10L] the high polling frequency for the dispatcher
#'     in milliseconds (used when there are active tasks). Setting a lower value
#'     will be more responsive but at the cost of consuming more resources on
#'     the dispatcher thread.
#' @param pollfreql [default 100L] the low polling frequency for the dispatcher
#'     in milliseconds (used when there are no active tasks). Setting a lower
#'     value will be more responsive but at the cost of consuming more resources
#'     on the dispatcher thread.
#' @param ... additional arguments passed through to \code{\link{server}} if
#'     launching local daemons i.e. 'url' is not specified.
#' @param monitor (for package internal use, not applicable if called
#'     independently) the client URL used for monitoring purposes as a character
#'     string.
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
dispatcher <- function(client, url = NULL, n = NULL, asyncdial = TRUE,
                       exitlinger = 100L, pollfreqh = 5L, pollfreql = 50L, ...,
                       monitor = NULL) {

  sock <- socket(protocol = "rep", dial = client, autostart = if (asyncdial) TRUE else NA)
  on.exit(expr = {
    msleep(exitlinger)
    close(sock)
  })

  auto <- is.null(url)
  n <- if (is.numeric(n)) as.integer(n) else length(url)
  n > 0L || stop("at least one URL must be supplied for 'url' or 'n' must be at least 1")
  vectorised <- length(url) == n
  seq_n <- seq_len(n)
  servernames <- character(n)
  instances <- activestore <- complete <- assigned <- integer(n)
  serverfree <- !integer(n)
  servers <- queue <- vector(mode = "list", length = n)

  ctrchannel <- is.character(monitor)
  if (ctrchannel) {
    sockc <- socket(protocol = "bus", dial = monitor, autostart = if (asyncdial) TRUE else NA)
    on.exit(expr = close(sockc), add = TRUE, after = FALSE)
    cmessage <- recv_aio(sockc, mode = 5L)
    attr(servernames, "dispatcher_pid") <- Sys.getpid()
  }

  if (!auto && !vectorised) {
    baseurl <- parse_url(url)
    ports <- if (grepl("tcp", baseurl[["scheme"]], fixed = TRUE))
      if (baseurl[["port"]] == "0") integer(n) else seq.int(baseurl[["port"]], length.out = n)
  }

  for (i in seq_n) {
    nurl <- if (auto) sprintf(.urlfmt, random()) else
      if (vectorised) url[i] else
        if (is.null(ports)) sprintf("%s/%d", url, i) else
          sub(ports[1L], ports[i], url, fixed = TRUE)
    nsock <- socket(protocol = "req", listen = nurl)
    if (i == 1L && !auto && parse_url(opt(attr(nsock, "listener")[[1L]], "url"))[["port"]] == "0") {
      realport <- opt(attr(nsock, "listener")[[1L]], "tcp-bound-port")
      nurl <- sub("(?<=:)0(?![^/])", realport, nurl, perl = TRUE)
      if (!vectorised) url <- sub("(?<=:)0(?![^/])", realport, url, perl = TRUE)
      close(nsock)
      nsock <- socket(protocol = "req", listen = nurl)
    }

    dotstring <- if (missing(...)) "" else
      sprintf(",%s", paste(names(dots <- substitute(alist(...))[-1L]), dots, sep = "=", collapse = ","))

    if (auto)
      launch_daemon(sprintf("mirai::server(\"%s\"%s)", nurl, dotstring))

    servernames[i] <- opt(attr(nsock, "listener")[[1L]], "url")
    servers[[i]] <- nsock
    ctx <- context(sock)
    req <- recv_aio(ctx, mode = 1L)
    queue[[i]] <- list(ctx = ctx, req = req)
  }

  devnull <- file(nullfile(), open = "w", blocking = FALSE)
  sink(file = devnull)
  sink(file = devnull, type = "message")
  on.exit(expr = {
    lapply(servers, close)
    sink()
    sink(type = "message")
    close(devnull)
  }, add = TRUE, after = TRUE)

  suspendInterrupts(
    repeat {

      activevec <- as.integer(unlist(lapply(servers, stat, "pipes")))
      changes <- (activevec - activestore) > 0L
      activestore <- activevec
      if (any(changes)) {
        assigned[changes] <- 0L
        complete[changes] <- 0L
        instances[changes] <- instances[changes] + 1L
      }

      active <- sum(activevec)
      free <- which(serverfree & activevec)

      msleep(if (length(free) == active) pollfreql else pollfreqh)

      ctrchannel && !unresolved(cmessage) && {
        data <- `attributes<-`(c(activevec, assigned - complete, assigned, complete, instances),
                               list(dim = c(n, 5L), dimnames = list(servernames, .statnames)))
        send(sockc, data = data, mode = 1L)
        cmessage <- recv_aio(sockc, mode = 5L)
        next
      }

      active || {
        for (i in seq_n)
          r <- .subset2(queue[[i]][["req"]], "data")
        next
      }

      if (length(free))
        for (q in free)
          for (i in seq_n) {
            if (length(queue[[i]]) == 2L && !unresolved(queue[[i]][["req"]])) {
              if (auto && active < n)
                for (j in which(!activevec)) launch_daemon(sprintf("mirai::server(\"%s\")", servernames[j]))
              ctx <- context(servers[[q]])
              queue[[i]][["rctx"]] <- ctx
              queue[[i]][["res"]] <- request(ctx, data = .subset2(queue[[i]][["req"]], "data"), send_mode = 1L, recv_mode = 1L)
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
          ctx <- context(sock)
          req <- recv_aio(ctx, mode = 1L)
          queue[[i]] <- list(ctx = ctx, req = req)
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
#' @param .expr an expression to evaluate asynchronously. This may be of
#'     arbitrary length, wrapped in \{\} if necessary.
#' @param ... (optional) named arguments specifying objects referenced in '.expr'.
#' @param .args (optional) list supplying objects referenced in '.expr' (used in
#'     addition to or instead of named arguments specified as '...').
#' @param .timeout (optional) integer value in milliseconds or NULL for no
#'     timeout. A mirai will resolve to an 'errorValue' 5 (timed out) if
#'     evaluation exceeds this limit.
#' @param .compute (optional) character value for the compute profile to use
#'     when sending the mirai.
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
#'     clean environment consisting only of the named objects passed as '...'
#'     and/or the list supplied to '.args'.
#'
#'     If an error occurs in evaluation, the error message is returned as a
#'     character string of class 'miraiError' and 'errorValue'.
#'     \code{\link{is_mirai_error}} may be used to test for this.
#'
#'     \code{\link{is_error_value}} tests for all error conditions including
#'     'mirai' errors, interrupts, and timeouts.
#'
#'     Specify '.compute' to send the mirai to a specific server destination, if
#'     multiple compute profiles have been set up via \code{\link{daemons}},
#'     otherwise leave as 'default'.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- mirai(x + y + 1, x = 2, y = 3)
#' m
#' m$data
#' Sys.sleep(0.2)
#' m$data
#'
#' df1 <- data.frame(a = 1, b = 2)
#' df2 <- data.frame(a = 3, b = 1)
#' m <- mirai(as.matrix(rbind(df1, df2)), .args = list(df1, df2), .timeout = 1000)
#' call_mirai(m)$data
#'
#' m <- mirai({
#'   res <- rnorm(n)
#'   res / rev(res)
#' }, n = 1e6)
#' while (unresolved(m)) {
#'   cat("unresolved\n")
#'   Sys.sleep(0.1)
#' }
#' str(m$data)
#'
#' file <- tempfile()
#' cat("r <- rnorm(n)", file = file)
#' n <- 10L
#' m <- mirai({source(file, local = TRUE); r}, .args = list(file, n))
#' call_mirai(m)[["data"]]
#' unlink(file)
#'
#' }
#'
#' @export
#'
mirai <- function(.expr, ..., .args = list(), .timeout = NULL, .compute = "default") {

  missing(.expr) && stop("missing expression, perhaps wrap in {}?")

  arglist <- list(.expr = substitute(.expr), ...)
  if (length(.args))
    arglist <- c(arglist, `names<-`(.args, as.character.default(substitute(.args)[-1L])))
  envir <- list2env(arglist, envir = NULL, parent = .GlobalEnv)

  if (length(..[[.compute]][["sock"]])) {
    aio <- request(context(..[[.compute]][["sock"]]),
                   data = envir, send_mode = 1L, recv_mode = 1L, timeout = .timeout)

  } else {
    url <- sprintf(.urlfmt, random())
    sock <- socket(protocol = "req", listen = url)
    launch_daemon(sprintf("mirai::.(\"%s\")", url))
    aio <- request(context(sock), data = envir, send_mode = 1L, recv_mode = 1L, timeout = .timeout)
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
#'     may take advantage of active dispatch, which ensures that tasks are
#'     assigned to servers efficiently on a FIFO basis, or else the low-level
#'     approach of distributing tasks to servers immediately in an even fashion.
#'
#' @param n integer number of daemons (server processes) to set.
#' @param url (optional) the client URL as a character vector, including a
#'     port accepting incoming connections and (optionally) a path for websocket
#'     URLs e.g. 'tcp://192.168.0.2:5555' or 'ws://192.168.0.2:5555/path'.
#' @param dispatcher [default TRUE] logical value whether to use a background
#'     dispatcher process. A dispatcher connects to servers on behalf of the
#'     client and queues tasks until a server is able to begin immediate
#'     execution of that task, ensuring FIFO scheduling (futher details below).
#' @param ... additional arguments passed through to \code{\link{dispatcher}} if
#'     using active dispatch or \code{\link{server}} if launching local daemons.
#' @param .compute (optional) character compute profile to use for creating the
#'     daemons (each compute profile can have its own set of daemons for
#'     connecting to different resources).
#'
#' @return Setting daemons: integer number of daemons set, or the character
#'     client URL.
#'
#'     Viewing current status: a named list comprising: \itemize{
#'     \item{\code{connections}} {- number of active connections at the client.
#'     Will always be 1L when using active dispatch as there is only one
#'     connection to the dispatcher, which then connects to the servers in turn.}
#'     \item{\code{daemons}} {- if using dispatcher: a matrix of statistics
#'     for each server: URL, online and busy status, cumulative tasks assigned
#'     and completed (reset if a server re-connects), and instance # (increments
#'     by 1 every time a server connects to the URL). If not using dispatcher:
#'     the number of daemons set, or else the client URL.}
#'     }
#'
#' @details For viewing the currrent status, specify \code{daemons()} with no
#'     arguments.
#'
#'     Use \code{daemons(0)} to reset all daemon connections at any time. This
#'     will send an exit signal to all connected daemons or dispatchers (and be
#'     propagated onwards where applicable) such that their processes exit. A
#'     reset is required before specifying revised daemons settings, otherwise
#'     these will not be registered.
#'
#'     After a reset, \{mirai\} will revert to the default behaviour of creating
#'     a new background process for each request.
#'
#'     When specifying a client URL, all daemons dialing into the client are
#'     detected automatically and resources may be added or removed at any time.
#'
#' @section Dispatcher:
#'
#'     By default \code{dispatcher = TRUE}. This launches a background process
#'     running \code{\link{dispatcher}}.  A dispatcher connects to servers on
#'     behalf of the client and queues tasks until a server is able to begin
#'     immediate execution of that task, ensuring FIFO scheduling.
#'
#'     By specifying \code{active = FALSE}, servers connect to the client
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
#'     Running local daemons with a dispatcher is self-repairing: in the case
#'     that daemons crash or are terminated, replacement daemons are launched
#'     upon the next task.
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
#'     \code{daemons()} to query the actual assigned port at any time.
#'
#'     \strong{With Dispatcher}
#'
#'     When using a dispatcher, it is recommended to use a websocket URL rather
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
#'     remote resource, which dial in to each of these client URLs.
#'
#'     Server instances may be scaled up or down dynamically, subject to the
#'     maximum number initially specified, with the dispatcher adjusting
#'     automatically.
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
#'     \code{\link{dispatcher}}) dial into the client, which listens at the
#'     client URL.
#'
#'     'n' does not need to be supplied in this case, and is disregarded if it
#'     is, as network resources may be added or removed at any time. The client
#'     automatically distributes tasks to all connected servers and dispatchers.
#'
#' @section Compute Profiles:
#'
#'     By default, the 'default' compute profile is used. Provide a character
#'     value for '.compute' to create a new compute profile with the name
#'     specified. Each compute profile retains its own daemons settings, and may
#'     be operated independently of each other. Some usage examples follow:
#'
#'     \strong{local / remote} new daemons may be set via a client URL and
#'     specifying '.compute' as 'remote'. This creates a new compute profile
#'     called 'remote'. Subsequent mirai calls may then be sent for local
#'     computation by not specifying its '.compute' argument, or for remote
#'     computation by specifying its '.compute' argument as 'remote'.
#'
#'     \strong{cpu / gpu} some tasks may require access to different classes of
#'     server, such as those with GPUs. In this case, \code{daemons()} may be
#'     called twice to set up client URLs for CPU-only and GPU servers to dial
#'     into respectively, specifying the '.compute' argument as 'cpu' and 'gpu'
#'     each time. By supplying the '.compute' argument to subsequent mirai calls,
#'     tasks may be sent to either 'cpu' or 'gpu' servers for computation.
#'
#'     Note: further actions such as viewing the status of daemons or resetting
#'     via \code{daemons(0)} should be carried out with the desired '.compute'
#'     argument specified.
#'
#' @section Timeouts:
#'
#'     Note: specifying the \code{.timeout} argument in \code{\link{mirai}} will
#'     ensure that the 'mirai' always resolves, however the process may not have
#'     completed and still be ongoing in the daemon. In such situations, using
#'     active dispatch ensures that queued tasks are not assigned to the busy
#'     process, however performance may still be degraded if they remain in use.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' # Create 2 local daemons (active dispatch)
#' daemons(2)
#' # View status
#' daemons()
#' # Reset to zero
#' daemons(0)
#'
#' # Create 2 local daemons (direct connection)
#' daemons(2, dispatcher = FALSE)
#' # View status
#' daemons()
#' # Reset to zero
#' daemons(0)
#'
#' # 2 remote servers via dispatcher (using zero wildcard)
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

  missing(n) && missing(url) &&
    return(list(connections = if (length(..[[.compute]][["sock"]])) stat(..[[.compute]][["sock"]], "pipes") else 0,
                daemons = if (length(..[[.compute]][["sockc"]])) query_nodes(.compute) else
                  if (length(..[[.compute]][["proc"]])) ..[[.compute]][["proc"]] else 0L))

  if (is.null(..[[.compute]])) `[[<-`(.., .compute, new.env(hash = FALSE, parent = environment(daemons)))

  if (is.character(url)) {

    if (is.null(..[[.compute]][["sock"]])) {
      if (dispatcher) {
        n <- if (missing(n)) length(url) else if (is.numeric(n) && n > 0L) as.integer(n) else
          stop("'n' must be 1 or greater if specified with a client URL")
        parse_url(url)
        urld <- sprintf(.urlfmt, random())
        urlc <- sprintf("%s%s", urld, "c")
        sock <- socket(protocol = "req", listen = urld)
        reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
        sockc <- socket(protocol = "bus", listen = urlc)
        dotstring <- if (missing(...)) "" else
          sprintf(",%s", paste(names(dots <- substitute(alist(...))[-1L]), dots, sep = "=", collapse = ","))
        args <- sprintf("mirai::dispatcher(\"%s\",c(%s),n=%d,monitor=\"%s\"%s)",
                        urld, paste(sprintf("\"%s\"", url), collapse = ","), n, urlc, dotstring)
        launch_daemon(args)
        `[[<-`(..[[.compute]], "sockc", sockc)
        proc <- n
      } else {
        sock <- socket(protocol = "req", listen = url)
        proc <- opt(attr(sock, "listener")[[1L]], "url")
        if (parse_url(proc)[["port"]] == "0")
          proc <- sub("(?<=:)0(?![^/])", opt(attr(sock, "listener")[[1L]], "tcp-bound-port"), proc, perl = TRUE)
        reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
      }
      `[[<-`(`[[<-`(`[[<-`(..[[.compute]], "sock", sock), "proc", proc), "timestamp", mclock())
    }

  } else {

    is.numeric(n) || stop("'n' must be numeric, did you mean to provide 'url'?")
    n <- as.integer(n)

    if (n == 0L) {
      length(..[[.compute]][["proc"]]) || return(0L)

      elapsed <- mclock() - ..[[.compute]][["timestamp"]]
      if (elapsed < 1000) msleep(1000 - elapsed)
      if (length(..[[.compute]][["sockc"]]))
        send(context(..[[.compute]][["sock"]]), data = .__scm__., mode = 2L, block = 1000L)
      close(..[[.compute]][["sock"]])
      `[[<-`(`[[<-`(`[[<-`(..[[.compute]], "sock", NULL), "sockc", NULL), "proc", NULL)
      gc(verbose = FALSE)

    } else if (is.null(..[[.compute]][["sock"]])) {

      n > 0L || stop("the number of daemons must be zero or greater")
      urld <- sprintf(.urlfmt, random())
      sock <- socket(protocol = "req", listen = urld)
      reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
      dotstring <- if (missing(...)) "" else
        sprintf(",%s", paste(names(dots <- substitute(alist(...))[-1L]), dots, sep = "=", collapse = ","))
      if (dispatcher) {
        urlc <- sprintf("%s%s", urld, "c")
        sockc <- socket(protocol = "bus", listen = urlc)
        args <- sprintf("mirai::dispatcher(\"%s\",n=%d,monitor=\"%s\"%s)", urld, n, urlc, dotstring)
        launch_daemon(args)
        `[[<-`(..[[.compute]], "sockc", sockc)
      } else {
        args <- sprintf("mirai::server(\"%s\"%s)", urld, dotstring)
        for (i in seq_len(n))
          launch_daemon(args)
      }
      `[[<-`(`[[<-`(`[[<-`(..[[.compute]], "sock", sock), "proc", n), "timestamp", mclock())
    }

  }

  if (length(..[[.compute]][["proc"]])) ..[[.compute]][["proc"]] else 0L

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
#' m <- mirai(x + y + 1, x = 2, y = 3)
#' m
#' m$data
#' Sys.sleep(0.2)
#' m$data
#'
#' df1 <- data.frame(a = 1, b = 2)
#' df2 <- data.frame(a = 3, b = 1)
#' m <- mirai(as.matrix(rbind(df1, df2)), .args = list(df1, df2), .timeout = 1000)
#' call_mirai(m)$data
#'
#' m <- mirai({
#'   res <- rnorm(n)
#'   res / rev(res)
#' }, n = 1e6)
#' while (unresolved(m)) {
#'   cat("unresolved\n")
#'   Sys.sleep(0.1)
#' }
#' str(m$data)
#'
#' file <- tempfile()
#' cat("r <- rnorm(n)", file = file)
#' n <- 10L
#' m <- mirai({source(file, local = TRUE); r}, .args = list(file, n))
#' call_mirai(m)[["data"]]
#' unlink(file)
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

query_nodes <- function(.compute) {
  send(..[[.compute]][["sockc"]], data = 0L, mode = 2L)
  recv(..[[.compute]][["sockc"]], mode = 1L, block = 1000L)
}

launch_daemon <- function(args)
  system2(command = .command, args = c("-e", shQuote(args)), stdout = NULL, stderr = NULL, wait = FALSE)

mk_mirai_error <- function(e) `class<-`(if (length(call <- .subset2(e, "call")))
  sprintf("Error in %s: %s", deparse(call, nlines = 1L), .subset2(e, "message")) else
    sprintf("Error: %s", .subset2(e, "message")), c("miraiError", "errorValue"))

mk_interrupt_error <- function(e) `class<-`("", c("miraiInterrupt", "errorValue"))

