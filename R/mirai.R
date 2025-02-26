# Copyright (C) 2022-2025 Hibiki AI Limited <info@hibiki-ai.com>
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
#' persistent daemon (local or remote). This function will return immediately
#' with a \sQuote{mirai}, which will resolve to the evaluated result once
#' complete.
#'
#' This function will return a \sQuote{mirai} object immediately.
#'
#' The value of a mirai may be accessed at any time at `$data`, and if yet
#' to resolve, an \sQuote{unresolved} logical NA will be returned instead.
#'
#' [unresolved()] may be used on a mirai, returning TRUE if a \sQuote{mirai} has
#' yet to resolve and FALSE otherwise. This is suitable for use in control flow
#' statements such as `while` or `if`.
#'
#' Alternatively, to call (and wait for) the result, use [call_mirai()] on the
#' returned \sQuote{mirai}. This will block until the result is returned.
#'
#' Specify `.compute` to send the mirai using a specific compute profile (if
#' previously created by [daemons()]), otherwise leave as \sQuote{default}.
#'
#' @param .expr an expression to evaluate asynchronously (of arbitrary length,
#'   wrapped in \{ \} where necessary), **or else** a pre-constructed language
#'   object.
#' @param ... (optional) **either** named arguments (name = value pairs)
#'   specifying objects referenced, but not defined, in `.expr`, **or** an
#'   environment containing such objects. See \sQuote{evaluation} section below.
#' @param .args (optional) **either** a named list specifying objects
#'   referenced, but not defined, in `.expr`, **or** an environment containing
#'   such objects. These objects will remain local to the evaluation environment
#'   as opposed to those supplied in `...` above - see \sQuote{evaluation}
#'   section below.
#' @param .timeout \[default NULL\] for no timeout, or an integer value in
#'   milliseconds. A mirai will resolve to an \sQuote{errorValue} 5 (timed out)
#'   if evaluation exceeds this limit.
#' @param .compute \[default 'default'\] character value for the compute profile
#'   to use (each compute profile has its own independent set of daemons).
#'
#' @return A \sQuote{mirai} object.
#'
#' @section Evaluation:
#'
#' The expression `.expr` will be evaluated in a separate R process in a clean
#' environment (not the global environment), consisting only of the objects
#' supplied to `.args`, with the objects passed as `...` assigned to the global
#' environment of that process.
#'
#' As evaluation occurs in a clean environment, all undefined objects must be
#' supplied though \sQuote{...} and/or \sQuote{.args}, including self-defined
#' functions. Functions from a package should use namespaced calls such as
#' `mirai::mirai()`, or else the package should be loaded beforehand as
#' part of \sQuote{.expr}.
#'
#' For evaluation to occur *as if* in your global environment, supply
#' objects to \sQuote{...} rather than \sQuote{.args}, e.g. for free variables
#' or helper functions defined in function bodies, as scoping rules may
#' otherwise prevent them from being found.
#'
#' @section Timeouts:
#'
#' Specifying the \sQuote{.timeout} argument ensures that the mirai always
#' resolves. However, the task may not have completed and still be ongoing in
#' the daemon process. Use [stop_mirai()] instead to explicitly stop
#' and interrupt a task.
#'
#' @section Errors:
#'
#' If an error occurs in evaluation, the error message is returned as a
#' character string of class \sQuote{miraiError} and \sQuote{errorValue}.
#' [is_mirai_error()] may be used to test for this. The elements of
#' the original condition are accessible via `$` on the error object. A
#' stack trace comprising a list of calls is also available at
#' `$stack.trace`.
#'
#' If a daemon crashes or terminates unexpectedly during evaluation, an
#' \sQuote{errorValue} 19 (Connection reset) is returned.
#'
#' [is_error_value()] tests for all error conditions including
#' \sQuote{mirai} errors, interrupts, and timeouts.
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
#' # passing the calling environment to '...'
#' df1 <- data.frame(a = 1, b = 2)
#' df2 <- data.frame(a = 3, b = 1)
#' m <- mirai(as.matrix(rbind(df1, df2)), environment(), .timeout = 1000)
#' m[]
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
#' # evaluating scripts using source() in '.expr'
#' n <- 10L
#' file <- tempfile()
#' cat("r <- rnorm(n)", file = file)
#' m <- mirai({source(file); r}, file = file, n = n)
#' call_mirai(m)$data
#' unlink(file)
#'
#' # use source(local = TRUE) when passing in local variables via '.args'
#' n <- 10L
#' file <- tempfile()
#' cat("r <- rnorm(n)", file = file)
#' m <- mirai({source(file, local = TRUE); r}, .args = list(file = file, n = n))
#' call_mirai(m)$data
#' unlink(file)
#'
#' # passing a language object to '.expr' and a named list to '.args'
#' expr <- quote(a + b + 2)
#' args <- list(a = 2, b = 3)
#' m <- mirai(.expr = expr, .args = args)
#' collect_mirai(m)
#'
#' }
#'
#' @export
#'
mirai <- function(.expr, ..., .args = list(), .timeout = NULL, .compute = "default") {

  missing(.expr) && stop(._[["missing_expression"]])

  expr <- substitute(.expr)
  globals <- list(...)
  length(globals) && {
    gn <- names(globals)
    if (is.null(gn)) {
      is.environment(globals[[1L]]) || stop(._[["named_dots"]])
      globals <- as.list.environment(globals[[1L]], all.names = TRUE)
      if (".Random.seed" %in% names(globals))
        globals[[".Random.seed"]] <- NULL
    }
    all(nzchar(gn)) || stop(._[["named_dots"]])
  }
  data <- list(
    ._mirai_globals_. = globals,
    .expr = if (is.symbol(expr) && exists(as.character(expr), envir = parent.frame()) && is.language(.expr)) .expr else expr
  )
  if (length(.args)) {
    if (is.environment(.args))
      .args <- as.list.environment(.args, all.names = TRUE) else
        length(names(.args)) && all(nzchar(names(.args))) || stop(._[["named_args"]])
    data <- c(.args, data)
  }

  envir <- ..[[.compute]]
  is.null(envir) && return(ephemeral_daemon(data, .timeout))
  r <- request(.context(envir[["sock"]]), data, send_mode = 1L, recv_mode = 1L, timeout = .timeout, cv = envir[["cv"]])
  `attr<-`(`attr<-`(r, "msgid", next_msgid(envir)), "profile", .compute)

}

#' Evaluate Everywhere
#'
#' Evaluate an expression \sQuote{everywhere} on all connected daemons for the
#' specified compute profile - this must be set prior to calling this function.
#' Designed for performing setup operations across daemons by loading packages
#' or exporting common data. Resultant changes to the global environment, loaded
#' packages and options are persisted regardless of a daemon's `cleanup`
#' setting.
#'
#' This function should be called when no other mirai operations are in
#' progress. If necessary, wait for all mirai operations to complete. This is as
#' this function does not force a synchronization point, and using concurrently
#' with other mirai operations does not guarantee the timing of when the
#' instructions will be received, or that they will be received on each daemon.
#'
#' @inheritParams mirai
#'
#' @return A list of mirai executed on each daemon. This may be waited for and
#'   inspected using [call_mirai()] or [collect_mirai()].
#'
#' @inheritSection mirai Evaluation
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' daemons(1)
#' # export common data by a super-assignment expression:
#' everywhere(y <<- 3)
#' # '...' variables are assigned to the global environment
#' # '.expr' may be specified as an empty {} in such cases:
#' everywhere({}, a = 1, b = 2)
#' m <- mirai(a + b - y == 0L)
#' m[]
#' # everywhere() returns a list of mirai which may be waited for and inspected
#' mlist <- everywhere("just a normal operation")
#' collect_mirai(mlist)
#' mlist <- everywhere(stop("error"))
#' collect_mirai(mlist)
#' daemons(0)
#'
#' # loading a package on all daemons
#' daemons(1, dispatcher = FALSE)
#' everywhere(library(parallel))
#' m <- mirai("package:parallel" %in% search())
#' m[]
#' daemons(0)
#'
#' }
#'
#' @export
#'
everywhere <- function(.expr, ..., .args = list(), .compute = "default") {

  envir <- ..[[.compute]]

  is.null(envir) && stop(sprintf(._[["not_found"]], .compute))

  expr <- substitute(.expr)
  .expr <- c(
    .snapshot,
    as.expression(if (is.symbol(expr) && exists(as.character(expr), envir = parent.frame()) && is.language(.expr)) .expr else expr)
  )

  if (is.null(envir[["msgid"]])) {
    vec <- vector(mode = "list", length = max(stat(envir[["sock"]], "pipes"), envir[["n"]]))
    for (i in seq_along(vec))
      vec[[i]] <- mirai(.expr, ..., .args = .args, .compute = .compute)
  } else {
    .expr <- c(.block, .expr)
    vec <- vector(mode = "list", length = max(status(.compute)[["connections"]], 1L))
    for (i in seq_along(vec))
      vec[[i]] <- mirai(.expr, ..., .args = .args, .compute = .compute)
  }
  `[[<-`(envir, "everywhere", vec)
  invisible(vec)

}

#' mirai (Call Value)
#'
#' Waits for the \sQuote{mirai} to resolve if still in progress, stores the
#' value at `$data`, and returns the \sQuote{mirai} object.
#'
#' Accepts a list of \sQuote{mirai} objects, such as that returned by
#' [mirai_map()] as well as individual \sQuote{mirai}.
#'
#' Waits for the asynchronous operation(s) to complete if still in progress,
#' blocking but user-interruptible.
#'
#' `x[]` may also be used to wait for and return the value of a mirai
#' `x`, and is the equivalent of `call_mirai(x)$data`.
#'
#' @param x a \sQuote{mirai} object, or list of \sQuote{mirai} objects.
#'
#' @return The passed object (invisibly). For a \sQuote{mirai}, the retrieved
#'   value is stored at `$data`.
#'
#' @section Alternatively:
#'
#' The value of a \sQuote{mirai} may be accessed at any time at `$data`,
#' and if yet to resolve, an \sQuote{unresolved} logical NA will be returned
#' instead.
#'
#' Using [unresolved()] on a \sQuote{mirai} returns TRUE only if it
#' has yet to resolve and FALSE otherwise. This is suitable for use in control
#' flow statements such as `while` or `if`.
#'
#' @inheritSection mirai Errors
#'
#' @note
#' `call_mirai_` is deprecated and exported for historical compatibility
#' only. It will be removed in a future package version.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' # using call_mirai()
#' df1 <- data.frame(a = 1, b = 2)
#' df2 <- data.frame(a = 3, b = 1)
#' m <- mirai(as.matrix(rbind(df1, df2)), df1 = df1, df2 = df2, .timeout = 1000)
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
#' }
#'
#' @export
#'
call_mirai <- call_aio_

#' @rdname call_mirai
#' @export
#'
call_mirai_ <- call_mirai

#' mirai (Collect Value)
#'
#' Waits for the \sQuote{mirai} to resolve if still in progress, and returns its
#' value directly. It is a more efficient version of and equivalent to
#' `call_mirai(x)$data`.
#'
#' This function will wait for the asynchronous operation(s) to complete if
#' still in progress, blocking but interruptible.
#'
#' `x[]` is an equivalent way to wait for and return the value of a mirai `x`.
#'
#' @inheritParams call_mirai
#' @param options (if \sQuote{x} is a list of mirai) a character vector
#'   comprising any combination of collection options for [mirai_map()], such as
#'   `".flat"` or `c(".progress", ".stop")`.
#'
#' @return An object (the return value of the \sQuote{mirai}), or a list of such
#'   objects (the same length as \sQuote{x}, preserving names).
#'
#' @inheritSection call_mirai Alternatively
#' @inheritSection mirai Errors
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' # using collect_mirai()
#' df1 <- data.frame(a = 1, b = 2)
#' df2 <- data.frame(a = 3, b = 1)
#' m <- mirai(as.matrix(rbind(df1, df2)), df1 = df1, df2 = df2, .timeout = 1000)
#' collect_mirai(m)
#'
#' # using x[]
#' m[]
#'
#' # mirai_map with collection options
#' daemons(1, dispatcher = FALSE)
#' m <- mirai_map(1:3, rnorm)
#' collect_mirai(m, c(".flat", ".progress"))
#' daemons(0)
#'
#' }
#'
#' @export
#'
collect_mirai <- function(x, options = NULL) {

  is.list(x) && length(options) || return(collect_aio_(x))

  ensure_cli_initialized()
  dots <- mget(options, envir = .)
  map(x, dots)

}

#' mirai (Stop)
#'
#' Stops a \sQuote{mirai} if still in progress, causing it to resolve
#' immediately to an \sQuote{errorValue} 20 (Operation canceled).
#'
#' Using dispatcher allows cancellation of \sQuote{mirai}. In the case that the
#' \sQuote{mirai} is awaiting execution, it is discarded from the queue and
#' never evaluated. In the case it is already in execution, an interrupt will be
#' sent.
#'
#' A successful cancellation request does not guarantee successful cancellation:
#' the task, or a portion of it, may have already completed before the interrupt
#' is received. Even then, compiled code is not always interruptible. This
#' should be noted, particularly if the code carries out side effects during
#' execution, such as writing to files, etc.
#'
#' @inheritParams call_mirai
#'
#' @return Logical TRUE if the cancellation request was successful (was awaiting
#'   execution or in execution), or else FALSE (if already completed or
#'   previously cancelled). Will always return FALSE if not using dispatcher.
#'
#'   Or a vector of logical values if supplying a list of \sQuote{mirai}, such
#'   as those returned by [mirai_map()].
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- mirai(Sys.sleep(n), n = 5)
#' stop_mirai(m)
#' m$data
#'
#' }
#'
#' @export
#'
stop_mirai <- function(x) {
  is.list(x) && {
    xlen <- length(x)
    vec <- logical(xlen)
    if (xlen)
      for (i in xlen:1)
        vec[i] <- stop_mirai(x[[i]])
    return(vec)
  }
  .compute <- attr(x, "profile")
  envir <- if (is.character(.compute)) ..[[.compute]]
  stop_aio(x)
  invisible(length(envir[["msgid"]]) && query_dispatcher(envir[["sock"]], c(0L, attr(x, "msgid"))))
}

#' Query if a mirai is Unresolved
#'
#' Query whether a \sQuote{mirai}, \sQuote{mirai} value or list of
#' \sQuote{mirai} remains unresolved. Unlike [call_mirai()], this
#' function does not wait for completion.
#'
#' Suitable for use in control flow statements such as `while` or
#' `if`.
#'
#' Note: querying resolution may cause a previously unresolved \sQuote{mirai} to
#' resolve.
#'
#' @param x a \sQuote{mirai} object or list of \sQuote{mirai} objects, or a
#'   \sQuote{mirai} value stored at `$data`.
#'
#' @return Logical TRUE if \sQuote{aio} is an unresolved \sQuote{mirai} or
#'   \sQuote{mirai} value or the list contains at least one unresolved
#'   \sQuote{mirai}, or FALSE otherwise.
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

#' Is mirai / mirai_map
#'
#' Is the object a \sQuote{mirai} or \sQuote{mirai_map}.
#'
#' @param x an object.
#'
#' @return Logical TRUE if \sQuote{x} is of class \sQuote{mirai} or
#'   \sQuote{mirai_map} respectively, FALSE otherwise.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' daemons(1, dispatcher = FALSE)
#' df <- data.frame()
#' m <- mirai(as.matrix(df), df = df)
#' is_mirai(m)
#' is_mirai(df)
#'
#' mp <- mirai_map(1:3, runif)
#' is_mirai_map(mp)
#' is_mirai_map(mp[])
#' daemons(0)
#'
#' }
#'
#' @export
#'
is_mirai <- function(x) inherits(x, "mirai")

#' @rdname is_mirai
#' @export
#'
is_mirai_map <- function(x) inherits(x, "mirai_map")

#' Error Validators
#'
#' Validator functions for error value types created by \pkg{mirai}.
#'
#' Is the object a \sQuote{miraiError}. When execution in a \sQuote{mirai}
#' process fails, the error message is returned as a character string of class
#' \sQuote{miraiError} and \sQuote{errorValue}. The elements of the original
#' condition are accessible via `$` on the error object. A stack trace is
#' also available at `$stack.trace`.
#'
#' Is the object a \sQuote{miraiInterrupt}. When an ongoing \sQuote{mirai} is
#' sent a user interrupt, it will resolve to an empty character string classed
#' as \sQuote{miraiInterrupt} and \sQuote{errorValue}.
#'
#' Is the object an \sQuote{errorValue}, such as a \sQuote{mirai} timeout, a
#' \sQuote{miraiError} or a \sQuote{miraiInterrupt}. This is a catch-all
#' condition that includes all returned error values.
#'
#' @param x an object.
#'
#' @return Logical value TRUE or FALSE.
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
#' m$data$stack.trace
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
`[.mirai` <- function(x, i) collect_aio_(x)

#' @export
#'
print.mirai <- function(x, ...) {

  cat(if (.unresolved(x)) "< mirai [] >\n" else "< mirai [$data] >\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.miraiError <- function(x, ...) {

  cat(sprintf("'miraiError' chr %s", x), file = stdout())
  invisible(x)

}

#' @export
#'
print.miraiInterrupt <- function(x, ...) {

  cat("'miraiInterrupt' chr \"\"\n", file = stdout())
  invisible(x)

}

#' @export
#'
`$.miraiError` <- function(x, name)
  attr(x, name, exact = TRUE)

#' @exportS3Method utils::.DollarNames
#'
.DollarNames.miraiError <- function(x, pattern = "")
  grep(pattern, names(attributes(x)), value = TRUE, fixed = TRUE)

# internals --------------------------------------------------------------------

ephemeral_daemon <- function(data, timeout) {
  url <- local_url()
  sock <- req_socket(url)
  system2(.command, args = c("-e", shQuote(sprintf("mirai:::.daemon(\"%s\")", url))), stdout = FALSE, stderr = FALSE, wait = FALSE)
  aio <- request(.context(sock), data, send_mode = 1L, recv_mode = 1L, timeout = timeout, cv = NA)
  `attr<-`(.subset2(aio, "aio"), "sock", sock)
  aio
}

deparse_safe <- function(x) if (length(x))
  deparse(x, width.cutoff = 500L, backtick = TRUE, control = NULL, nlines = 1L)

mk_interrupt_error <- function() .miraiInterrupt

mk_mirai_error <- function(cnd, sc) {
  cnd[["call"]] <- `attributes<-`(.subset2(cnd, "call"), NULL)
  call <- deparse_safe(.subset2(cnd, "call"))
  msg <- if (is.null(call) || call == "eval(._mirai_.[[\".expr\"]], envir = ._mirai_., enclos = .GlobalEnv)")
    sprintf("Error: %s", .subset2(cnd, "message")) else
      sprintf("Error in %s: %s", call, .subset2(cnd, "message"))
  idx <- max(
    which(
      as.logical(
        lapply(sc, `==`, "eval(._mirai_.[[\".expr\"]], envir = ._mirai_., enclos = .GlobalEnv)")
      )
    )
  )
  sc <- sc[(length(sc) - 1L):(idx + 1L)]
  if (sc[[1L]][[1L]] == ".handleSimpleError")
    sc <- sc[-1L]
  sc <- lapply(sc, `attributes<-`, NULL)
  out <- `attributes<-`(msg, `[[<-`(cnd, "stack.trace", sc))
  `class<-`(out, c("miraiError", "errorValue", "try-error"))
}

.miraiInterrupt <- `class<-`("", c("miraiInterrupt", "errorValue", "try-error"))
.connectionReset <- `class<-`(19L, c("errorValue", "try-error"))
.snapshot <- expression(on.exit(mirai:::snapshot(), add = TRUE))
.block <- expression(on.exit(nanonext::msleep(500L), add = TRUE))
