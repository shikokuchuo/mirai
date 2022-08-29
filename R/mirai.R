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

#' mirai Server (Async Executor)
#'
#' Implements an executor/server for the remote process. Awaits data, evaluates
#'     an expression in an environment containing the supplied data, and returns
#'     the result to the caller/client.
#'
#' @param . the internally assigned unique URL.
#'
#' @return Integer exit code.
#'
#' @noRd
#'
. <- function(.) {

  sock <- socket(protocol = "rep", dial = .)
  ctx <- context(sock)
  on.exit(expr = {
    send(ctx, data = `class<-`(geterrmessage(), c("miraiError", "errorValue")), mode = 1L, echo = FALSE)
    close(sock)
  })
  envir <- recv(ctx, mode = 1L, keep.raw = FALSE)
  msg <- eval(expr = .subset2(envir, ".expr"), envir = envir)
  send(ctx, data = msg, mode = 1L, echo = FALSE)
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
#' }
#'
#' @export
#'
eval_mirai <- function(.expr, ..., .args = list(), .timeout = NULL) {

  missing(.expr) && stop("missing expression, perhaps wrap in {}?")
  if (!is.null(proc <- attr(daemons(), "daemons")) && proc) {

    arglist <- list(.expr = substitute(.expr), ...)
    if (length(.args))
      arglist <- c(arglist, `names<-`(.args, as.character.default(substitute(.args)[-1L])))
    envir <- list2env(arglist)
    ctx <- context(daemons())
    aio <- request(ctx, data = envir, send_mode = 1L, recv_mode = 1L, timeout = .timeout, keep.raw = FALSE)
    `attr<-`(.subset2(aio, "aio"), "ctx", ctx)
    `class<-`(aio, c("mirai", "recvAio"))

  } else {

    arglist <- list(.expr = substitute(.expr), ...)
    if (length(.args))
      arglist <- c(arglist, `names<-`(.args, as.character.default(substitute(.args)[-1L])))
    envir <- list2env(arglist)
    url <- switch(.sysname,
                  Linux = sprintf("abstract://n%.f", random()),
                  sprintf("ipc:///tmp/n%.f", random()))
    arg <- c("--vanilla", "-e", shQuote(sprintf("mirai:::.(%s)", deparse(url))))
    cmd <- switch(.sysname,
                  Windows = file.path(R.home("bin"), "Rscript.exe"),
                  file.path(R.home("bin"), "Rscript"))
    system2(command = cmd, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
    sock <- socket(protocol = "req", listen = url)
    ctx <- context(sock)
    aio <- request(ctx, data = envir, send_mode = 1L, recv_mode = 1L, timeout = .timeout, keep.raw = FALSE)
    `attr<-`(.subset2(aio, "aio"), "ctx", ctx)
    `attr<-`(.subset2(aio, "aio"), "sock", sock)
    `class<-`(aio, c("mirai", "recvAio"))

  }

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
#' @param aio a 'mirai' (mirai are also aio objects).
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
#' }
#'
#' @export
#'
call_mirai <- call_aio

#' mirai Server (Async Execution Daemon)
#'
#' Implements a persistent executor/server for the remote process. Awaits data,
#'     evaluates an expression in an environment containing the supplied data,
#'     and returns the result to the caller/client.
#'
#' @inheritParams .
#'
#' @return Integer exit code.
#'
#' @noRd
#'
.. <- function(.) {

  sock <- socket(protocol = "rep", dial = .)
  ctx <- context(sock)
  on.exit(expr = {
    send(ctx, data = `class<-`(geterrmessage(), c("miraiError", "errorValue")), mode = 1L, echo = FALSE)
    close(sock)
    rm(list = ls())
    ..(.)
  })

  repeat {
    envir <- recv(ctx, mode = 1L, keep.raw = FALSE)
    missing(envir) && break
    msg <- eval(expr = .subset2(envir, ".expr"), envir = envir)
    send(ctx, data = msg, mode = 1L, echo = FALSE)
    close(ctx)
    ctx <- context(sock)
  }

  on.exit()
  close(sock)

}

#' mirai (Stop Evaluation)
#'
#' Stop evaluation of a mirai that is in progress.
#'
#' @param aio a 'mirai' (mirai are also aio objects).
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

#' daemons (Background Processes)
#'
#' Set or view the number of daemons (background processes). Create persistent
#'     background processes to receive \code{\link{mirai}} requests. This
#'     provides a potentially more efficient solution for async operations as
#'     new processes no longer need to be created on an ad hoc basis.
#'
#' @param ... either an integer to set the number of daemons, or 'view' to view
#'     the number of currently active daemons.
#'
#' @return Depending on the specified \code{...} parameter:
#'     \itemize{
#'     \item{integer: integer change in number of daemons (created or destroyed).}
#'     \item{'view': integer number of currently set daemons.}
#'     \item{missing: the 'nanoSocket' for connecting to the daemons, or NULL if
#'     it is yet to be created.}
#'     }
#'
#' @details \{mirai\} will revert to the default behaviour of creating a new
#'     background process for each request if the number of daemons is set to 0.
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
daemons <- function(...) {

  proc <- 0L
  url <- sock <- cmd <- arg <- NULL

  function(...) {

    if (missing(...)) {
      sock

    } else if (is.numeric(..1)) {
      if (length(..1) > 1L) {
        set <- as.integer(..1[1L])
        warning("vector specified, only using first element")
      } else {
        set <- as.integer(..1)
      }
      set >= 0L || stop("number of daemons must be zero or greater")
      delta <- set - proc
      delta == 0L && return(0L)

      if (is.null(url)) {
        url <<- switch(.sysname,
                       Linux = sprintf("abstract://n%.f", random()),
                       sprintf("ipc:///tmp/n%.f", random()))
        sock <<- socket(protocol = "req", listen = url)
        arg <<- c("--vanilla", "-e", shQuote(sprintf("mirai:::..(%s)", deparse(url))))
        cmd <<- switch(.sysname,
                       Windows = file.path(R.home("bin"), "Rscript.exe"),
                       file.path(R.home("bin"), "Rscript"))
        reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
      }
      if (delta > 0L) {
        orig <- proc
        for (i in seq_len(delta)) {
          system2(command = cmd, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
          proc <<- proc + 1L
        }
        attr(sock, "daemons") <- proc
        proc - orig
      } else {
        halt <- 0L
        for (i in seq_len(-delta)) {
          ctx <- context(sock)
          res <- send_aio(ctx, data = .mirai_scm(), mode = 1L, timeout = 2000L)
          if (.subset2(call_aio(res), "result")) {
            warning(sprintf("daemon %d shutdown failed", i))
          } else {
            halt <- halt - 1L
            proc <<- proc - 1L
          }
          close(ctx)
        }
        attr(sock, "daemons") <- proc
        halt
      }

    } else if (is.character(..1) && ..1 == "view") {
      if (is.null(d <- attr(sock, "daemons"))) 0L else d

    } else {
      stop("specify an integer value to set daemons or 'view' to view daemons")
    }
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

