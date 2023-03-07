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
#' @param url the client URL as a character string, including the port to
#'     connect to and (optionally) a path for websocket URLs e.g.
#'     'tcp://192.168.0.2:5555' or 'ws://192.168.0.2:5555/path'.
#' @param nodes [default NULL] if supplied, this server instance will run as an
#'     active queue (task scheduler) with the specified number of nodes.
#' @param idletime [default Inf] maximum idle time, since completion of the last
#'     task (in milliseconds) before exiting.
#' @param walltime [default Inf] soft walltime, or the minimum amount of real
#'     time taken (in milliseconds) before exiting.
#' @param tasklimit [default Inf] the maximum number of tasks to execute (task
#'     limit) before exiting.
#' @param asyncdial [default TRUE] (for debugging purposes) whether to dial in
#'     to the client asynchronously. An asynchronous dial is more resilient and
#'     will continue retrying if not immediately successful, however this can
#'     mask potential connection issues. If FALSE, will error if a connection is
#'     not immediately possible (e.g. \code{\link{daemons}} has yet to be called
#'     on the client, or the specified port is not open etc.).
#' @param ... reserved but not currently used.
#' @param pollfreqh [default 5L] applicable for an active queue only, the high
#'     polling frequency for the queue in milliseconds (used when there are
#'     active tasks). Setting a lower value will be more responsive but at the
#'     cost of consuming more resources on the queue thread.
#' @param pollfreql [default 50L] applicable for an active queue only, the low
#'     polling frequency for the queue in milliseconds (used when there are no
#'     active tasks). Setting a lower value will be more responsive but at the
#'     cost of consuming more resources on the queue thread.
#'
#' @return Invisible NULL.
#'
#' @details The network topology is such that server daemons dial into the
#'     client, which listens at the '.url' address. In this way, network
#'     resources may be easily added or removed at any time and the client
#'     automatically distributes tasks to all available servers.
#'
#'     If 'nodes' is supplied, this daemon is launched as an active server queue,
#'     directing a cluster with the specified number of nodes. The nodes are
#'     launched automatically as processes on the same machine.
#'
#'     An active server queue may be used in combination with other servers or
#'     server queues.
#'
#' @export
#'
server <- function(url, nodes = NULL, idletime = Inf, walltime = Inf, tasklimit = Inf,
                   asyncdial = TRUE, ..., pollfreqh = 5L, pollfreql = 50L) {

  sock <- socket(protocol = "rep", dial = url, autostart = if (asyncdial) TRUE else NA)
  devnull <- file(nullfile(), open = "w", blocking = FALSE)
  sink(file = devnull)
  sink(file = devnull, type = "message")
  on.exit(expr = {
    close(sock)
    sink()
    sink(type = "message")
    close(devnull)
  })
  count <- 0L
  idle <- FALSE
  start <- mclock()

  if (is.numeric(nodes)) {

    ctrchannel <- length(url) > 1L
    auto <- length(url) < 3L
    nodes <- as.integer(nodes)
    vectorised <- length(url) == nodes + 2L
    seq_nodes <- seq_len(nodes)
    servernames <- character(nodes)
    serverfree <- !integer(nodes)
    assigned <- integer(nodes)
    complete <- integer(nodes)
    queue <- vector(mode = "list", length = nodes)
    servers <- vector(mode = "list", length = nodes)

    if (ctrchannel) {
      sockc <- socket(protocol = "bus", dial = url[2L], autostart = if (asyncdial) TRUE else NA)
      on.exit(expr = close(sockc), add = TRUE, after = FALSE)
      controlq <- recv_aio(sockc, mode = 5L)
    }

    if (!auto && !vectorised) {
      baseurl <- parse_url(url[3L])
      ports <- if (grepl("tcp", baseurl[["scheme"]], fixed = TRUE)) as.character(seq.int(baseurl[["port"]], length.out = nodes))
    }

    for (i in seq_nodes) {
      nurl <- if (auto) sprintf(.urlfmt, random()) else
        if (vectorised) url[i + 2L] else
          if (is.null(ports)) sprintf("%s/%d", url[3L], i) else
            sub(ports[1L], ports[i], url[3L], fixed = TRUE)
      nsock <- socket(protocol = "req", listen = nurl)
      if (!auto && parse_url(opt(attr(nsock, "listener")[[1L]], "url"))[["port"]] == "0") {
        realport <- opt(attr(nsock, "listener")[[1L]], "tcp-bound-port")
        nurl <- sub("(?<=:)0(?![^/])", realport, nurl, perl = TRUE)
        if (!vectorised) url[3L] <- sub("(?<=:)0(?![^/])", realport, url[3L], perl = TRUE)
        close(nsock)
        nsock <- socket(protocol = "req", listen = nurl)
      }

      if (auto)
        launch_daemon(sprintf("mirai::server(\"%s\")", nurl))

      servernames[i] <- opt(attr(nsock, "listener")[[1L]], "url")
      servers[[i]] <- nsock

      ctx <- context(sock)
      req <- recv_aio(ctx, mode = 1L)
      queue[[i]] <- list(ctx = ctx, req = req)
    }

    on.exit(expr = for (i in seq_nodes) {
      send(servers[[i]], data = .__scm__., mode = 2L)
      close(servers[[i]])
    }, add = TRUE)

    suspendInterrupts({

      while (count < tasklimit && mclock() - start < walltime && if (idle) mclock() - idle < idletime else TRUE) {

        activevec <- as.integer(unlist(lapply(servers, stat, "pipes")))
        assigned <- activevec * assigned
        complete <- activevec * complete
        active <- sum(activevec)
        free <- which(serverfree & activevec)
        if (length(free) == active) {
          if (active && !idle) idle <- mclock()
          msleep(pollfreql)
        } else {
          if (idle) idle <- FALSE
          msleep(pollfreqh)
        }

        ctrchannel && !unresolved(controlq) && {
          data <- cbind(status_active = activevec, tasks_assigned = assigned, tasks_complete = complete)
          dimnames(data)[[1L]] <- servernames
          send(sockc, data = data, mode = 1L)
          controlq <- recv_aio(sockc, mode = 5L)
          next
        }

        active || {
          for (i in seq_nodes)
            r <- .subset2(queue[[i]][["req"]], "data")
          next
        }

        if (length(free))
          for (q in free)
            for (i in seq_nodes)
              if (length(queue[[i]]) == 2L && !unresolved(queue[[i]][["req"]])) {
                if (auto && active < nodes)
                  for (j in which(!activevec)) launch_daemon(sprintf("mirai::server(\"%s\")", servernames[[j]]))
                ctx <- context(servers[[q]])
                queue[[i]][["rctx"]] <- ctx
                queue[[i]][["res"]] <- request(ctx, data = .subset2(queue[[i]][["req"]], "data"), send_mode = 1L, recv_mode = 1L)
                queue[[i]][["daemon"]] <- q
                serverfree[q] <- FALSE
                assigned[q] <- assigned[q] + 1L
                break
              }

        for (i in seq_nodes)
          if (length(queue[[i]]) > 2L && !unresolved(queue[[i]][["res"]])) {
            send(queue[[i]][["ctx"]], data = .subset2(queue[[i]][["res"]], "data"), mode = 1L)
            q <- queue[[i]][["daemon"]]
            serverfree[q] <- TRUE
            complete[q] <- complete[q] + 1L
            count <- count + 1L
            ctx <- context(sock)
            req <- recv_aio(ctx, mode = 1L)
            queue[[i]] <- list(ctx = ctx, req = req)
          }

      }
    })

  } else {
    if (idletime == Inf) idletime <- NULL
    while (count < tasklimit && mclock() - start < walltime) {

      ctx <- context(sock)
      envir <- recv(ctx, mode = 1L, block = idletime)
      is.integer(envir) && break
      data <- tryCatch(eval(expr = envir[[".expr"]], envir = envir, enclos = NULL),
                       error = mk_mirai_error, interrupt = mk_interrupt_error)
      send(ctx, data = data, mode = 1L)
      count <- count + 1L

    }
  }

  msleep(2000L)

}

#' @noRd
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
#' @param .compute (optional) character value for the compute profile to use.
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
#'     Specify '.compute' if multiple compute profiles have been set up via
#'     \code{\link{daemons}}, otherwise leave as 'default'.
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
#' Set 'daemons' or persistent server processes receiving \code{\link{mirai}}
#'     requests. These are, by default, automatically created on the local
#'     machine. Alternatively, a client URL may be set to receive connections
#'     from remote servers started with \code{\link{server}} for distributing
#'     tasks across the network.
#'
#' @param value \emph{~ depending on the type of value supplied ~}
#'
#'     \strong{numeric}: for setting local daemons: integer number of daemons
#'     (see 'Local Daemons' below).
#'
#'     \strong{character}: for distributing tasks across the network: the client
#'     URL as a character string, including a port accepting incoming connections
#'     and (optionally) a path for websocket URLs e.g. 'tcp://192.168.0.2:5555'
#'     or 'ws://192.168.0.2:5555/path'. (see 'Distributed Computing' below).
#'
#'     \strong{missing}: for viewing the currrent status, specify
#'     \code{daemons()} with no arguments.
#'
#' @param ... additional named arguments passed to \code{\link{server}}.
#'
#'     \strong{nodes} supplying an integer number of nodes runs an active queue
#'     with the specified number of nodes.
#' @param .compute (optional) character compute profile to use for creating the
#'     daemons (each compute profile can have its own set of daemons for
#'     connecting to different resources).
#'
#' @return Setting daemons: integer number of daemons set, or the character
#'     client URL (1L if specifying 'nodes').
#'
#'     Viewing current status: a named list comprising: \itemize{
#'     \item{\code{connections}} {- number of active connections.}
#'     \item{\code{daemons}} {- number of daemons, or the client URL when
#'     running a passive queue.}
#'     \item{\code{nodes}} {- a named vector of the number of connected nodes at
#'     each client URL when running an active queue, or else NA.}
#'     }
#'
#' @details Use \code{daemons(0)} to reset all daemon connections at any time.
#'     \{mirai\} will revert to the default behaviour of creating a new
#'     background process for each request.
#'
#'     When specifying a client URL, all daemons dialing into the client are
#'     detected automatically and resources may be added or removed dynamically.
#'     Further specifying a numeric number of daemons has no effect, with the
#'     exception that \code{daemons(0)} will always reset and attempt to
#'     shutdown all connected daemons.
#'
#'     Setting a new client URL will attempt to shutdown all daemons connected
#'     at the existing address before opening a connection at the new address.
#'
#' @section Local Daemons:
#'
#'     Daemons provide a potentially more efficient solution for asynchronous
#'     operations as new processes no longer need to be created on an \emph{ad
#'     hoc} basis.
#'
#'     The default implementation is low-level and ensures tasks are
#'     evenly-distributed amongst daemons. This provides a robust and
#'     resource-light approach, particularly suited to working with
#'     similar-length tasks, or where the number of concurrent tasks typically
#'     does not exceed available daemons.
#'
#'     Alternatively, supplying \code{nodes} as an additional argument launches
#'     an active queue with the specified number of nodes e.g.
#'     \code{daemons(1, nodes = 8)}. When 'nodes' is specified, the value for
#'     daemons is disregarded and one active queue is launched. An active queue
#'     consumes additional resources, however ensures load balancing and optimal
#'     scheduling of tasks to nodes. Note that changing the number of nodes in
#'     an active queue requires a reset to zero prior to specifying a revised
#'     number.
#'
#' @section Distributed Computing:
#'
#'     \strong{Passive Queues}
#'
#'     Specifying a client URL allows tasks to be distributed across the network.
#'
#'     This should be in the form of a character string such as:
#'     'tcp://192.168.0.2:5555' at which server processes started using
#'     \code{\link{server}} should connect to. Alternatively, to listen to port
#'     5555 on all interfaces on the local host, specify either 'tcp://:5555',
#'     'tcp://*:5555' or 'tcp://0.0.0.0:5555'.
#'
#'     Specifying the wildcard value zero for the port number e.g. 'tcp://:0' or
#'     'ws://:0' will automatically assign a free ephemeral port. Use
#'     \code{daemons()} to query the actual assigned port at any time.
#'
#'     The network topology is such that server daemons (started with
#'     \code{\link{server}}) dial into the client, which listens at the client
#'     URL. In this way, network resources may be easily added or removed at any
#'     time. The client automatically distributes tasks to all connected servers.
#'
#'     \strong{Active Queues}
#'
#'     Supplying \code{nodes} as an additional argument will launch a local
#'     daemon as an active server queue.
#'
#'     It is recommended to use a websocket URL instead of TCP in this case so
#'     that only one port is used to connect to the nodes. This is as a websocket
#'     URL supports a path after the port number, which can be made unique for
#'     each node. Specifying a single client URL such as 'ws://192.168.0.2:5555'
#'     with 6 nodes will automatically append a sequence to the path, listening
#'     to the URLs 'ws://192.168.0.2:5555/1' through 'ws://192.168.0.2:5555/6'.
#'
#'     Alternatively, specify a vector of URLs the same length as 'nodes' to
#'     listen to arbitrary port numbers / paths.
#'
#'     Individual \code{\link{server}} instances should then be started on the
#'     remote resource, with each of these specified as the client URL.
#'
#'     Server nodes may be scaled up or down dynamically, subject to the maximum
#'     'nodes' initially specified, with the queue automatically adjusting.
#'
#'     Alternatively, supplying a single TCP URL will listen on a block of URLs
#'     with ports starting from the supplied port number and incrementing by one
#'     for the number of nodes specified e.g. the client URL
#'     'tcp://192.168.0.2:5555' with 6 nodes listens to the contiguous block of
#'     ports 5555 through 5560.
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
#'     completed and still be ongoing in the daemon. In such situations, an
#'     active queue may be preferable so that new tasks are not assigned to the
#'     busy process, however performance may still be degraded if they remain in
#'     use.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' # Create 2 daemons
#' daemons(2)
#'
#' # View status
#' daemons()
#'
#' # Reset to zero
#' daemons(0)
#'
#' # Create 1 active queue with 2 nodes
#' daemons(1, nodes = 2)
#'
#' # View status
#' daemons()
#'
#' # Reset to zero
#' daemons(0)
#'
#' }
#'
#' @export
#'
daemons <- function(value, ..., .compute = "default") {

  missing(value) &&
    return(list(connections = if (length(..[[.compute]][["sock"]])) stat(..[[.compute]][["sock"]], "pipes") else 0,
                daemons = if (length(..[[.compute]][["proc"]])) ..[[.compute]][["proc"]] else 0L,
                nodes = if (length(..[[.compute]][["sockc"]])) query_nodes(.compute) else NA))

  if (is.null(..[[.compute]])) `[[<-`(.., .compute, new.env(hash = FALSE, parent = environment(daemons)))

  is.numeric(value) || {

    is.character(value) || stop("'value' must be numeric, character or missing")
    if (length(..[[.compute]][["sock"]])) daemons(0L)
    nodes <- NULL
    if (...length()) {
      dots <- substitute(alist(...))[-1L]
      nodes <- as.integer(.subset2(dots, "n", exact = FALSE))
    }
    if (length(nodes)) {
      url <- sprintf(.urlfmt, random())
      urlc <- sprintf("%s%s", url, "c")
      sock <- socket(protocol = "req", listen = url)
      reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
      sockc <- socket(protocol = "bus", listen = urlc)
      dotstring <- paste(names(dots), dots, sep = "=", collapse = ",")
      args <- sprintf("mirai::server(c(%s),%s)",
                      paste(sprintf("\"%s\"", c(url, urlc, value)), collapse = ","),
                      dotstring)
      launch_daemon(args)
      `[[<-`(`[[<-`(..[[.compute]], "sockc", sockc), "args", args)
      proc <- 1L
    } else {
      sock <- socket(protocol = "req", listen = value)
      proc <- opt(attr(sock, "listener")[[1L]], "url")
      if (parse_url(proc)[["port"]] == "0")
        proc <- sub("(?<=:)0(?![^/])", opt(attr(sock, "listener")[[1L]], "tcp-bound-port"), proc, perl = TRUE)
      reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
    }
    `[[<-`(`[[<-`(`[[<-`(..[[.compute]], "sock", sock), "local", FALSE), "proc", proc)
    return(proc)

  }

  n <- as.integer(value)
  n >= 0L || stop("the number of daemons must be zero or greater")
  proc <- if (length(..[[.compute]][["proc"]])) ..[[.compute]][["proc"]] else 0L
  delta <- if (is.integer(proc)) n - proc else -1L
  delta == 0L && return(proc)

  local <- is.null(..[[.compute]][["local"]])

  if (is.null(..[[.compute]][["sock"]])) {
    url <- sprintf(.urlfmt, random())
    sock <- socket(protocol = "req", listen = url)
    reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
    if (...length()) {
      dots <- substitute(alist(...))[-1L]
      dotstring <- paste(names(dots), dots, sep = "=", collapse = ",")
      nodes <- .subset2(dots, "n", exact = FALSE)
      if (length(nodes)) {
        delta <- 1L
        urlc <- sprintf("%s%s", url, "c")
        sockc <- socket(protocol = "bus", listen = urlc)
        args <- sprintf("mirai::server(c(\"%s\",\"%s\"),%s)", url, urlc, dotstring)
        `[[<-`(`[[<-`(..[[.compute]], "sockc", sockc), "local", TRUE)
      } else {
        args <- sprintf("mirai::server(\"%s\",%s)", url, dotstring)
      }
    } else {
      args <- sprintf("mirai::server(\"%s\")", url)
    }
    `[[<-`(`[[<-`(..[[.compute]], "sock", sock), "args", args)
  }

  if (delta > 0L) {
    if (local) {
      for (i in seq_len(delta))
        launch_daemon(..[[.compute]][["args"]])
      proc <- proc + delta
      `[[<-`(..[[.compute]], "proc", proc)
    }

  } else {
    if (n == 0L || local) {
      proc <- as.integer(stat(..[[.compute]][["sock"]], "pipes"))
      delta <- if (n == 0L) proc else min(-delta, proc)
      for (i in seq_len(delta))
        send(context(..[[.compute]][["sock"]]), data = .__scm__., mode = 2L, block = 1000L)
      proc <- proc - delta
      `[[<-`(..[[.compute]], "proc", proc)
    }
    if (proc == 0L) {
      close(..[[.compute]][["sock"]])
      `[[<-`(`[[<-`(`[[<-`(..[[.compute]], "sock", NULL), "sockc", NULL), "local", NULL)
      gc(verbose = FALSE)
    }
  }

  proc

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

