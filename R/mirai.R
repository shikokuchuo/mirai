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

#' mirai (Evaluate Async)
#'
#' Evaluate an expression asynchronously in a new background R process or
#'     persistent daemon (local or remote). This function will return
#'     immediately with a 'mirai', which will resolve to the evaluated result
#'     once complete.
#'
#' @param .expr an expression to evaluate asynchronously (of arbitrary length,
#'     wrapped in \{\} if necessary), \strong{or} a language object passed by
#'     \link{name}.
#' @param ... (optional) named arguments (name = value pairs) specifying
#'     objects referenced in '.expr'. Used in addition to, and taking precedence
#'     over, any arguments specified via '.args'.
#' @param .args (optional) \strong{either} a list of objects to be passed by
#'     \link{name} (found in the current scope), \strong{or else} a list of
#'     name = value pairs, as in '...'. If an object other than a list is
#'     supplied, it will be coerced to a list.
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
  arglist <- list(..., .expr = if (is.symbol(expr) && is.language(get0(as.character(expr), envir = sys.frame(-1L)))) .expr else expr)

  if (length(.args)) {
    if (!is.list(.args)) .args <- as.list(.args)
    arglist <- if (length(names(.args))) c(.args, arglist) else
      c(`names<-`(.args, as.character(substitute(.args)[-1L])), arglist)
  }

  data <- list2env(arglist, envir = NULL, parent = .GlobalEnv)

  envir <- ..[[.compute]]
  if (length(envir)) {
    aio <- if (is.raw(.timeout)) request(.context(envir[["sock"]]), data = data, send_mode = 3L, recv_mode = 1L) else
      request_signal(.context(envir[["sock"]]), data = data, cv = envir[["cv"]], send_mode = 3L, recv_mode = 1L, timeout = .timeout)

  } else {
    url <- auto_tokenized_url()
    sock <- req_socket(url, resend = 0L)
    length(.timeout) && { launch_and_sync_daemon(sock = sock, url) || return(.connection_error) } || launch_daemon(url)
    aio <- request(.context(sock), data = data, send_mode = 1L, recv_mode = 1L, timeout = .timeout)
    `attr<-`(.subset2(aio, "aio"), "sock", sock)

  }

  `class<-`(aio, c("mirai", "recvAio"))

}

#' Evaluate Everywhere
#'
#' Evaluate an expression 'everywhere' on all connected daemons for the
#'     specified compute profile. Designed for performing setup operations
#'     across daemons, resultant changes to the global environment, loaded
#'     packages or options are persisted regardless of a daemon's 'cleanup'
#'     setting.
#'
#' @inheritParams mirai
#'
#' @return Invisible NULL.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' daemons(1)
#' everywhere(list2env(x, envir = .GlobalEnv), x = list(a = 1, b = 2))
#' m <- mirai(a + b)
#' call_mirai(m)$data
#' daemons(0)
#'
#' daemons(1, dispatcher = FALSE)
#' everywhere(library(parallel))
#' m <- mirai("package:parallel" %in% search())
#' call_mirai(m)$data
#' daemons(0)
#'
#' }
#'
#' @export
#'
everywhere <- function(.expr, ..., .args = list(), .compute = "default") {

  envir <- ..[[.compute]]
  length(envir) || return(invisible())

  expr <- c(as.expression(substitute(.expr)), .snapshot)

  if (length(envir[["sockc"]])) {
    expr <- c(expr, .timedelay)
    for (i in seq_len(envir[["n"]]))
      mirai(.expr = expr, ..., .args = .args, .compute = .compute)
  } else {
    for (i in seq_len(max(stat(envir[["sock"]], "pipes"), envir[["n"]])))
      mirai(.expr = expr, ..., .args = .args, .compute = .compute)
  }

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
#' Validator functions for error value types created by \pkg{mirai}.
#'
#' @param x an object.
#'
#' @return Logical value TRUE or FALSE.
#'
#' @details Is the object a 'miraiError'. When execution in a mirai process fails,
#'     the error message is returned as a character string of class 'miraiError'
#'     and 'errorValue'.
#'
#'     Is the object a 'miraiInterrupt'. When an ongoing mirai is sent a user
#'     interrupt, the mirai will resolve to an empty character string classed as
#'     'miraiInterrupt' and 'errorValue'.
#'
#'     Is the object an 'errorValue', such as a mirai timeout, a 'miraiError' or
#'     a 'miraiInterrupt'. This is a catch-all condition that includes all
#'     returned error values.
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

#' Register Custom Serialization Functions
#'
#' For sending and receiving reference objects accessed via an external pointer.
#'
#' @param inhook a function (for custom serialization). The signature for this
#'     function must accept a list and return a raw vector, e.g.
#'     \code{torch::torch_serialize}, or else NULL to reset.
#' @param outhook a function (for custom unserialization). The signature for
#'     this function must accept a raw vector and return a list, e.g.
#'     \code{torch::torch_load}, or else NULL to reset.
#'
#' @return Invisibly, a pairlist comprising the currently-registered 'inhook'
#'     and 'outhook' functions.
#'
#' @details For the functions to be registered, both 'inhook' and 'outhook' need
#'     to be specified. Calling without any arguments returns (invisibly) the
#'     currently-registered functions.
#'
#' @export
#'
register <- function(inhook, outhook) {

  if (!missing(inhook) && !missing(outhook))
    for (.compute in names(..))
      everywhere(mirai::register(inhook, outhook), inhook = inhook, outhook = outhook, .compute = .compute)

  nextmode(inhook, outhook)

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

  cat(sprintf("'miraiError' chr %s\n", x), file = stdout())
  invisible(x)

}

#' @export
#'
print.miraiInterrupt <- function(x, ...) {

  cat("'miraiInterrupt' chr \"\"\n", file = stdout())
  invisible(x)

}

# internals --------------------------------------------------------------------

mk_interrupt_error <- function(e) .interrupt_error

mk_mirai_error <- function(e) {
  x <- .subset2(e, "call")
  call <- if (length(x)) deparse(x, width.cutoff = 500L, backtick = TRUE, control = NULL, nlines = 1L)
  msg <- if (is.null(call) || call == "eval(expr = ._mirai_.[[\".expr\"]], envir = ._mirai_., enclos = NULL)")
    strcat("Error: ", .subset2(e, "message")) else
      sprintf("Error in %s: %s", call, .subset2(e, "message"))
  cat(strcat(msg, "\n"), file = stderr());
  `class<-`(msg, c("miraiError", "errorValue", "try-error"))
}

snapshot <- function() `[[<-`(`[[<-`(`[[<-`(., 'vars', names(.GlobalEnv)), 'se', search()), 'op', .Options)

.connection_error <- list(data = `class<-`(6L, c("errorValue", "try-error")))
.interrupt_error <- `class<-`("", c("miraiInterrupt", "errorValue", "try-error"))
.snapshot <- expression(mirai:::snapshot())
.timedelay <- expression(nanonext::msleep(500L))
