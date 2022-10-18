# Copyright (C) 2022 Hibiki AI Limited <info@hibiki-ai.com>
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
#' @param .. [default TRUE] launch as a persistent daemon or, if FALSE, an
#'     ephemeral process.
#'
#' @return Integer exit code, zero upon success.
#'
#' @export
#'
. <- function(.url, .. = TRUE) {

  sock <- socket(protocol = "rep", dial = .url)

  if (..)
    repeat {
      on.exit(expr = close(sock))
      ctx <- context(sock)
      envir <- recv(ctx, mode = 1L)
      on.exit(expr = {
        send(ctx, data = `class<-`(geterrmessage(), .errorclass), mode = 1L)
        close(sock)
        rm(list = ls())
        .(.url)
      })
      data <- eval(expr = .subset2(envir, ".expr"), envir = envir)
      send(ctx, data = data, mode = 1L)
      close(ctx)
    }

  ctx <- context(sock)
  on.exit(expr = {
    send(ctx, data = `class<-`(geterrmessage(), .errorclass), mode = 1L)
    close(sock)
  })
  envir <- recv(ctx, mode = 1L)
  data <- eval(expr = .subset2(envir, ".expr"), envir = envir)
  send(ctx, data = data, mode = 1L)
  on.exit()
  msleep(2000L)
  close(sock)

}

#' mirai (Evaluate Async)
#'
#' Evaluate an expression asynchronously in a new background R process. This
#'     function will return immediately with a 'mirai', which will resolve to
#'     the evaluated result once complete.
#'
#' @param .expr an expression to evaluate in a new R process. This may be of
#'     arbitrary length, wrapped in \{\} if necessary.
#' @param ... (optional) named arguments specifying variables contained in '.expr'.
#' @param .args (optional) list supplying arguments to '.expr' (used in addition
#'     to or instead of named arguments specified as '...').
#' @param .timeout (optional) integer value in milliseconds or NULL for no
#'     timeout. A 'mirai' will resolve to an 'errorValue' 5 (timed out) if
#'     evaluation exceeds this limit.
#'
#' @return A 'mirai' object.
#'
#' @details This function will return a 'mirai' object immediately.
#'
#'     The value of a 'mirai' may be accessed at any time at \code{$data}, and
#'     if yet to resolve, an 'unresolved' logical NA will be returned instead.
#'
#'     \code{\link{unresolved}} may also be used on a 'mirai', which returns TRUE
#'     only if a 'mirai' has yet to resolve and FALSE otherwise. This is suitable
#'     for use in control flow statements such as \code{while} or \code{if}.
#'
#'     Alternatively, to call (and wait for) the result, use
#'     \code{\link{call_mirai}} on the returned 'mirai' object. This will block
#'     until the result is returned.
#'
#'     The expression '.expr' will be evaluated in a new R process in a clean
#'     environment consisting of the named objects passed as '...' only (along
#'     with objects in the list '.args', if supplied).
#'
#'     If an error occurs in evaluation, the error message is returned as a
#'     character string of class 'miraiError' and 'errorValue'.
#'     \code{\link{is_mirai_error}} may be used to test for this, otherwise
#'     \code{\link{is_error_value}} will also include other errors such as
#'     timeouts.
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
    system2(command = .command,
            args = c("--vanilla", "-e", shQuote(sprintf("mirai::.(%s,FALSE)", deparse(url)))),
            stdout = NULL, stderr = NULL, wait = FALSE)
    sock <- socket(protocol = "req", listen = url)
    ctx <- context(sock)
    aio <- request(ctx, data = list2env(arglist), send_mode = 1L, recv_mode = 1L, timeout = .timeout)
    `attr<-`(`attr<-`(.subset2(aio, "aio"), "ctx", ctx), "sock", sock)
  }

  `class<-`(aio, .miraiclass)

}

#' @rdname eval_mirai
#' @export
#'
mirai <- eval_mirai

#' mirai (Call Value)
#'
#' Call the value of a 'mirai', waiting for the the asynchronous operation to
#'     resolve if it is still in progress.
#'
#' @param aio a 'mirai' (also an 'aio' object).
#'
#' @return The passed 'mirai' (invisibly). The retrieved value is stored at \code{$data}.
#'
#' @details This function will wait for the async operation to complete if still
#'     in progress (blocking).
#'
#'     If an error occurs in evaluation, the error message is returned as a
#'     character string of class 'miraiError' and 'errorValue'.
#'     \code{\link{is_mirai_error}} may be used to test for this, otherwise
#'     \code{\link{is_error_value}} will also include other errors such as
#'     timeouts.
#'
#'     The 'mirai' updates itself in place, so to access the value of a 'mirai'
#'     \code{x} directly, use \code{call_mirai(x)$data}.
#'
#' @section Alternatively:
#'
#'     The value of a 'mirai' may be accessed at any time at \code{$data}, and
#'     if yet to resolve, an 'unresolved' logical NA will be returned instead.
#'
#'     \code{\link{unresolved}} may also be used on a 'mirai', and returns TRUE
#'     only if a 'mirai' has yet to resolve and FALSE otherwise. This is suitable
#'     for use in control flow statements such as \code{while} or \code{if}.
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
#' @param aio a 'mirai' (also an 'aio' object).
#'
#' @return Invisible NULL.
#'
#' @details Stops the asynchronous operation associated with 'mirai' by aborting,
#'     and then waits for it to complete or to be completely aborted. The 'mirai'
#'     is then deallocated and attempting to access the value at \code{$data}
#'     will result in an error.
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

#' Is mirai
#'
#' Is the object a mirai.
#'
#' @param x an object.
#'
#' @return Logical value TRUE or FALSE.
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

#' daemons (Background and Remote Processes)
#'
#' Set or view the number of daemons (server processes). Create persistent
#'     background processes to receive \code{\link{mirai}} requests, providing
#'     an efficient solution for async operations on a local machine. Also
#'     provides the interface for distributing requests across the network.
#'
#' @param n integer number of daemons to set | 'view' to view the current number
#'     of daemons.
#' @param .url (optional) for distributing tasks across the network: character
#'     client URL and port accepting incoming connections e.g.
#'     'tcp://192.168.0.2:5555' at which server processes started using \code{.()}
#'     should connect to. To listen to port 5555 (for example) on all interfaces
#'     on the host, specify one of 'tcp://:5555', 'tcp://*:5555' or
#'     'tcp://0.0.0.0:5555'.
#'
#' @return Depending on 'n' specified:
#'     \itemize{
#'     \item{integer: integer change in number of daemons (created or destroyed).}
#'     \item{'view': integer number of currently set daemons.}
#'     }
#'
#' @details Set 'n' to 0 to reset all daemon connections. \{mirai\} will revert
#'     to the default behaviour of creating a new background process for each
#'     request.
#'
#'     Specifying a custom client URL without 'n' (or 'n' < 1) will default to a
#'     value for 'n' of 1.
#'
#'     Calling \code{daemons()} without any arguments returns the 'nanoSocket'
#'     for connecting to the daemons, or NULL if it is yet to be created.
#'
#' @section About:
#'
#'     Daemons provide a potentially more efficient solution for async operations
#'     as new processes no longer need to be created on an ad hoc basis.
#'
#'     Specifying '.url' also allows tasks to be distributed across the network.
#'     The network togology is that server daemons dial into the client socket,
#'     such that network resources may be easily added or removed at any time.
#'
#'     The current implementation is low-level and ensures tasks are
#'     evenly-distributed amongst daemons without actively managing a task queue.
#'     This approach provides a robust and resource-light solution, particularly
#'     well-suited to working with similar-length tasks, or where the number of
#'     concurrent tasks typically does not exceed the number of available
#'     daemons.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' # Create 4 daemons
#' daemons(4)
#' # View the number of active daemons
#' daemons("view")
#' # Reset to zero
#' daemons(0)
#'
#' }
#'
#' @export
#'
daemons <- function(n, .url) {

  proc <- 0L
  url <- sock <- arg <- NULL
  local <- TRUE

  function(n, .url) {

    if (missing(.url)) {
      missing(n) && return(sock)
      is.character(n) && n == "view" && return(proc)

    } else if (is.character(.url)) {
      if (missing(n) || n < 1L)
        n <- 1L
      if (length(sock))
        daemons(0L)
      sock <<- socket(protocol = "req", listen = .url)
      reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
      local <<- FALSE

    } else {
      stop("invalid input - non-character value supplied for '.url'")
    }

    is.numeric(n) || stop("invalid input - non-numeric value supplied for 'n'")
    if (length(n) > 1L) {
      n <- n[1L]
      warning("vector specified for 'n', only using first element")
    }
    n >= 0L || stop("'n' must be zero or greater")

    delta <- as.integer(n) - proc
    delta == 0L && return(delta)

    if (is.null(sock)) {
      url <<- sprintf(.urlfmt, random())
      sock <<- socket(protocol = "req", listen = url)
      reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
      arg <<- c("--vanilla", "-e", shQuote(sprintf("mirai::.(%s)", deparse(url))))
      local <<- TRUE
    }

    if (delta > 0L) {
      if (local) {
        for (i in seq_len(delta))
          system2(command = .command, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
      }
      proc <<- proc + delta

    } else {
      out <- 0L
      for (i in seq_len(-delta)) {
        ctx <- context(sock)
        res <- send_aio(ctx, data = .__scm__., mode = 2L, timeout = 2000L)
        if (suppressWarnings(.subset2(call_aio(res), "result")))
          out <- out + 1L
        close(ctx)
      }
      proc <<- proc + delta
      if (out)
        warning(sprintf("%d daemon shutdowns timed out (may require manual action)", out))
      if (proc == 0L) {
        close(sock)
        sock <<- NULL
        gc(verbose = FALSE, full = TRUE)
      }
    }

    delta

  }
}

#' @export
#'
print.mirai <- function(x, ...) {

  cat("< mirai >\n - $data for evaluated result\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.miraiError <- function(x, ...) {

  cat(x, file = stderr())
  invisible(x)

}

#' Is mirai Error
#'
#' Is the object a 'miraiError'. When execution in a mirai process fails, the
#'     error message is returned as a character string of class 'miraiError' and
#'     'errorValue'. To test for all errors, including timeouts etc.,
#'     \code{\link{is_error_value}} should be used instead.
#'
#' @param x an object.
#'
#' @return Logical value TRUE if 'x' is of class 'miraiError', FALSE otherwise.
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

#' Query if a Mirai is Unresolved
#'
#' Query whether a mirai or mirai value remains unresolved. Unlike
#'     \code{\link{call_mirai}}, this function does not wait for completion.
#'
#' @param aio A 'mirai' or mirai value stored in \code{$data} (mirai are also
#'     aio objects).
#'
#' @return Logical TRUE or FALSE.
#'
#' @details Returns TRUE for unresolved mirai or mirai values, FALSE otherwise.
#'
#'     Suitable for use in control flow statements such as \code{while} or \code{if}.
#'
#'     Note: querying resolution may cause a previously unresolved mirai to resolve.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- mirai(Sys.sleep(0.1))
#' unresolved(m)
#' Sys.sleep(0.5)
#' unresolved(m)
#'
#' }
#'
#' @export
#'
unresolved <- unresolved

#' Is Error Value
#'
#' Is the object an error value generated by the system or a 'miraiError' from
#'     failed execution within a mirai. Includes user-specified errors such as
#'     mirai timeouts.
#'
#' @param x an object.
#'
#' @return Logical value TRUE if 'x' is of class 'errorValue', FALSE otherwise.
#'
#' @examples
#' is_error_value(1L)
#'
#' @export
#'
is_error_value <- is_error_value

