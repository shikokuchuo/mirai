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

#' mirai Server (Async Executor [Daemon])
#'
#' Implements a [persistent] executor/server for the remote process. Awaits data,
#'     evaluates an expression in an environment containing the supplied data,
#'     and returns the result to the caller/client.
#'
#' @param .url the client URL and port to connect to as a character string e.g.
#'     'tcp://192.168.0.2:5555'.
#' @param daemon [default TRUE] launch as a persistent daemon or, if FALSE, an
#'     ephemeral process.
#'
#' @return Invisible NULL.
#'
#' @section About:
#'
#'     The network topology is such that server daemons dial into the client,
#'     which listens at the '.url' address. In this way, network resources may
#'     be added or removed at any time and the client automatically distributes
#'     tasks to all available servers.
#'
#' @export
#'
server <- function(.url, daemon = TRUE) {

  sock <- socket(protocol = "rep", dial = .url)
  on.exit(expr = close(sock))

  repeat {
    ctx <- context(sock)
    envir <- recv(ctx, mode = 1L)
    data <- tryCatch(eval(expr = .subset2(envir, ".expr"), envir = envir, enclos = NULL),
                     error = mk_mirai_error, interrupt = mk_interrupt_error)
    send(ctx, data = data, mode = 1L)
    close(ctx)
    daemon || break
  }

  msleep(2000L)

}

#' mirai Server Queue
#'
#' Implements an active queue / task scheduler, launching and directing a cluster
#'     of daemons.
#'
#' @param n integer number of daemons to set.
#' @inheritParams server
#'
#' @return Invisible NULL.
#'
#' @section About:
#'
#'     The network topology is such that this server queue dials into the client,
#'     which listens at the '.url' address. A server queue launches and directs
#'     a cluster of 'n' daemons and relays messages back and forth from the
#'     client. A server queue may be used in combination with other servers or
#'     server queues and the client automatically distributes tasks to all
#'     connected resources.
#'
#' @export
#'
serverq <- function(n, .url) {

  sock <- socket(protocol = "rep", dial = .url)
  on.exit(expr = close(sock))
  queue <- vector(mode = "list", length = n)
  servers <- vector(mode = "list", length = n)

  for (i in seq_len(n)) {
    url <- sprintf(.urlfmt, random())
    socko <- socket(protocol = "req", listen = url)
    system2(command = .command,
            args = c("--vanilla", "-e", shQuote(sprintf("mirai::server(%s)", deparse(url)))),
            stdout = NULL, stderr = NULL, wait = FALSE)
    servers[[i]] <- list(url = url, sock = socko, free = TRUE)
    ctx <- context(sock)
    req <- recv_aio(ctx, mode = 1L)
    queue[[i]] <- list(ctx = ctx, req = req)
  }

  on.exit(expr = for (i in seq_len(n)) {
    send(servers[[i]][["sock"]], data = .__scm__., mode = 2L)
    close(servers[[i]][["sock"]])
  }, add = TRUE)

  repeat {

    free <- which(unlist(lapply(servers, .subset2, "free")))

    msleep(if (length(free) == n) 50L else 5L)

    if (length(free))
      for (q in free)
        for (i in seq_len(n))
          if (length(queue[[i]]) == 2L && !unresolved(queue[[i]][["req"]])) {
            ctx <- context(servers[[q]][["sock"]])
            queue[[i]][["rctx"]] <- ctx
            queue[[i]][["res"]] <- request(ctx, data = queue[[i]][["req"]][["data"]], send_mode = 1L, recv_mode = 1L)
            queue[[i]][["daemon"]] <- q
            servers[[q]][["free"]] <- FALSE
            break
          }

    for (i in seq_len(n))
      if (length(queue[[i]]) > 2L && !unresolved(queue[[i]][["res"]])) {
        send(queue[[i]][["ctx"]], data = queue[[i]][["res"]][["data"]], mode = 1L)
        q <- queue[[i]][["daemon"]]
        servers[[q]][["free"]] <- TRUE
        ctx <- context(sock)
        req <- recv_aio(ctx, mode = 1L)
        queue[[i]] <- list(ctx = ctx, req = req)
      }

  }

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
#'     \code{\link{mirai}} is an alias for \code{\link{eval_mirai}}.
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
eval_mirai <- function(.expr, ..., .args = list(), .timeout = NULL) {

  missing(.expr) && stop("missing expression, perhaps wrap in {}?")

  arglist <- list(.expr = substitute(.expr), ...)
  if (length(.args))
    arglist <- c(arglist, `names<-`(.args, as.character.default(substitute(.args)[-1L])))

  if (length(daemons())) {
    ctx <- context(daemons())
    aio <- request(ctx, data = list2env(arglist), send_mode = 1L, recv_mode = 1L, timeout = .timeout)
    `attr<-`(.subset2(aio, "aio"), "ctx", ctx)

  } else {
    url <- sprintf(.urlfmt, random())
    sock <- socket(protocol = "req", listen = url)
    system2(command = .command,
            args = c("--vanilla", "-e", shQuote(sprintf("mirai::server(%s,0)", deparse(url)))),
            stdout = NULL, stderr = NULL, wait = FALSE)
    ctx <- context(sock)
    aio <- request(ctx, data = list2env(arglist), send_mode = 1L, recv_mode = 1L, timeout = .timeout)
    `attr<-`(`attr<-`(.subset2(aio, "aio"), "ctx", ctx), "sock", sock)
  }

  `class<-`(aio, c("mirai", "recvAio"))

}

#' @rdname eval_mirai
#' @export
#'
mirai <- eval_mirai

#' daemons (Persistent Server Processes)
#'
#' Set 'daemons' or persistent server processes receiving \code{\link{mirai}}
#'     requests. These are, by default, automatically created on the local
#'     machine. Alternatively, a client URL may be set to receive connections
#'     from remote servers started with \code{\link{server}} or
#'     \code{\link{serverq}} for distributing tasks across the network.
#'
#' @param ... \emph{(depending on the type of argument supplied)}
#'
#'     \strong{numeric}: integer number of local daemons to set.
#'
#'     \strong{character}: for distributing tasks across the network: the client
#'     URL and port accepting incoming connections e.g. 'tcp://192.168.0.2:5555'
#'     at which server processes started using \code{\link{server}} or
#'     \code{\link{serverq}} should connect to. For example to listen to port
#'     5555 on all interfaces on the host, specify either 'tcp://:5555',
#'     'tcp://*:5555' or 'tcp://0.0.0.0:5555'.
#'
#' @param q [default FALSE] (applicable only for local daemons) logical value
#'     whether to maintain an active queue. This requires resources to maintain,
#'     however ensures optimal allocation of tasks to daemons (see section 'Local
#'     Daemons' below).
#'
#' @return Integer number of daemons set (1L if supplying a client URL).
#'     Calling \code{daemons()} without any arguments returns the 'nanoSocket'
#'     for connecting to the daemons, or NULL if it is yet to be created.
#'
#' @details Use \code{daemons(0)} to reset all daemon connections at any time.
#'     \{mirai\} will revert to the default behaviour of creating a new
#'     background process for each request.
#'
#'     When specifying a client URL, all daemons dialing into the client are
#'     detected automatically and resources may be added or removed dynamically.
#'     Further specifying a numeric number of daemons has no effect, with the
#'     exception that \code{daemons(0)} will always attempt to shutdown all
#'     connected daemons.
#'
#'     Setting a new client URL will attempt to shutdown all daemons connected
#'     at the existing address before opening a connection at the new address.
#'
#' @section Local Daemons:
#'
#'     Daemons provide a potentially more efficient solution for asynchronous
#'     operations as new processes no longer need to be created on an ad hoc
#'     basis.
#'
#'     The default implementation with \code{q = FALSE} is low-level and ensures
#'     tasks are evenly-distributed amongst daemons. This provides a robust and
#'     resource-light approach, particularly suited to working with
#'     similar-length tasks, or where the number of concurrent tasks typically
#'     does not exceed available daemons.
#'
#'     Alternatively, specifying \code{q = TRUE} maintains an active queue. This
#'     consumes additional resources, however ensures optimal allocation of
#'     tasks to daemons such that they are run as soon as resources become
#'     available. Note that modifying the number of daemons in an active queue
#'     requires a reset to zero prior to specifying a revised number.
#'
#' @section Distributed Computing:
#'
#'     Specifying a client URL allows tasks to be distributed across the network.
#'     The network topology is such that server daemons (started with
#'     \code{\link{server}} or \code{\link{serverq}}) dial into the client,
#'     which listens at the client URL. In this way, network resources may
#'     be added or removed at any time. The client automatically distributes
#'     tasks to all connected servers.
#'
#' @section Timeouts:
#'
#'     Note: specifying the \code{.timeout} argument when evaluating a 'mirai'
#'     will always cause the user function to return, however the process may not
#'     have completed and still be ongoing in the daemon. In such situations, an
#'     active queue may be prefereable so that new tasks are not assigned to
#'     those daemons, however performance will still be degraded if they remain
#'     in use.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' # Create 2 daemons
#' daemons(2)
#' # Reset to zero
#' daemons(0)
#'
#' }
#'
#' @export
#'
daemons <- function(..., q) {

  proc <- 0L
  url <- sock <- arg <- NULL
  local <- TRUE

  function(..., q = FALSE) {

    ...length() || return(sock)

    if (is.numeric(..1)) {
      n <- as.integer(..1)
      n >= 0L || stop("the number of daemons must be zero or greater")
      delta <- n - proc

    } else if (is.character(..1)) {
      if (length(sock))
        daemons(0L)
      sock <<- socket(protocol = "req", listen = ..1)
      reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
      proc <<- delta <- 1L
      local <<- FALSE
    } else {
      stop("a numeric or character value must be supplied for '...'")
    }

    delta == 0L && return(proc)

    if (is.null(sock)) {
      url <<- sprintf(.urlfmt, random())
      sock <<- socket(protocol = "req", listen = url)
      reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
      if (q) {
        arg <<- c("--vanilla", "-e", shQuote(sprintf("mirai::serverq(%d,%s)", n, deparse(url))))
        system2(command = .command, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
        proc <<- n
        local <<- FALSE
      } else {
        arg <<- c("--vanilla", "-e", shQuote(sprintf("mirai::server(%s)", deparse(url))))
        local <<- TRUE
      }
    }

    if (delta > 0L) {
      if (local) {
        for (i in seq_len(delta))
          system2(command = .command, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
        proc <<- proc + delta
      }

    } else {
      if (!local && n == 0L) {
        proc <<- as.integer(stat(sock, "pipes"))
        delta <- -proc
        local <<- TRUE
      }
      if (local) {
        out <- 0L
        for (i in seq_len(-delta)) {
          ctx <- context(sock)
          res <- send(ctx, data = .__scm__., mode = 2L, block = 2000L)
          if (res)
            out <- out + 1L
          close(ctx)
        }
        proc <<- proc + delta
        if (out)
          warning(sprintf("%d daemon shutdowns timed out (may require manual action)", out))
      }
      if (proc == 0L) {
        close(sock)
        sock <<- NULL
        gc(verbose = FALSE)
      }
    }

    proc

  }
}

#' View Daemons
#'
#' View the number of currently active 'daemons' or persistent server processes.
#'
#' @return A named list comprising: \itemize{
#'     \item{\code{daemons}} {- integer number of daemons set.}
#'     \item{\code{connections}} {- integer number of active connections at the
#'     client URL.}
#'     }
#'
#' @details Note: for an active queue, the number of connections will always be
#'     1L as only the queue connects to the client. When using a client URL, the
#'     number of daemons will always show as 1L, however the connections will
#'     reflect the number of actual connected servers.
#'
#' @examples
#' daemons_view()
#'
#' @export
#'
daemons_view <- function()
  list(daemons = .subset2(environment(daemons), "proc"),
       connections = if (length(daemons())) as.integer(stat(daemons(), "pipes")) else 0L)

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

#' Is Error Value
#'
#' Is the object an error value, such as a mirai timeout, a 'miraiError' from
#'     failed execution within a mirai or a 'miraiInterrupt' resulting from
#'     the user interrupt of an ongoing mirai evaluation.
#'
#' @param x an object.
#'
#' @return Logical TRUE if 'x' is of class 'errorValue', FALSE otherwise.
#'
#' @examples
#' is_error_value(1L)
#'
#' @export
#'
is_error_value <- is_error_value

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
#' m <- mirai(as.matrix(df), df = data.frame())
#' is_mirai(m)
#' is_mirai(df)
#'
#' }
#'
#' @export
#'
is_mirai <- function(x) inherits(x, "mirai")

#' Is mirai Error
#'
#' Is the object a 'miraiError'. When execution in a mirai process fails, the
#'     error message is returned as a character string of class 'miraiError' and
#'     'errorValue'. To test for all error conditions, including timeouts etc.,
#'     \code{\link{is_error_value}} should be used instead.
#'
#' @param x an object.
#'
#' @return Logical TRUE if 'x' is of class 'miraiError', FALSE otherwise.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- mirai(stop())
#' call_mirai(m)
#' is_mirai_error(m$data)
#'
#' }
#'
#' @export
#'
is_mirai_error <- function(x) inherits(x, "miraiError")

#' Is mirai Interrupt
#'
#' Is the object a 'miraiInterrupt'. When a mirai is sent a user interrupt,
#'     e.g. by ctrl+c during an ongoing \code{\link{call_mirai}}, the mirai
#'     will resolve to an empty character string classed as 'miraiInterrupt' and
#'     'errorValue'. To test for all error conditions, including timeouts etc.,
#'     \code{\link{is_error_value}} should be used instead.
#'
#' @param x an object.
#'
#' @return Logical TRUE if 'x' is of class 'miraiInterrupt', FALSE otherwise.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- mirai(stop())
#' call_mirai(m)
#' is_mirai_interrupt(m$data)
#'
#' }
#'
#' @export
#'
is_mirai_interrupt <- function(x) inherits(x, "miraiInterrupt")

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

mk_mirai_error <- function(e) `class<-`(if (length(call <- .subset2(e, "call")))
  sprintf("Error in %s: %s", deparse(call, nlines = 1L), .subset2(e, "message")) else
    sprintf("Error: %s", .subset2(e, "message")), c("miraiError", "errorValue"))

mk_interrupt_error <- function(e) `class<-`("", c("miraiInterrupt", "errorValue"))

