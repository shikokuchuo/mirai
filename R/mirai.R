# Copyright (C) 2022-2024 Hibiki AI Limited <info@hibiki-ai.com>
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
#'     immediately with a \sQuote{mirai}, which will resolve to the evaluated
#'     result once complete.
#'
#' @param .expr an expression to evaluate asynchronously (of arbitrary length,
#'     wrapped in \{ \} where necessary), \strong{or else} a pre-constructed
#'     language object.
#' @param ... (optional) \strong{either} named arguments (name = value pairs)
#'     specifying objects referenced, but not defined, in \sQuote{.expr},
#'     \strong{or} an environment containing such objects. See
#'     \sQuote{evaluation} section below.
#' @param .args (optional) \strong{either} a named list specifying objects
#'     referenced, but not defined, in \sQuote{.expr}, \strong{or} an
#'     environment containing such objects. These objects will remain local to
#'     the evaluation environment as opposed to those supplied in \sQuote{...}
#'     above - see \sQuote{evaluation} section below.
#' @param .timeout [default NULL] for no timeout, or an integer value in
#'     milliseconds. A mirai will resolve to an \sQuote{errorValue} 5 (timed
#'     out) if evaluation exceeds this limit.
#' @param .compute [default 'default'] character value for the compute profile
#'     to use (each compute profile has its own independent set of daemons).
#'
#' @return A \sQuote{mirai} object.
#'
#' @details This function will return a \sQuote{mirai} object immediately.
#'
#'     The value of a mirai may be accessed at any time at \code{$data}, and
#'     if yet to resolve, an \sQuote{unresolved} logical NA will be returned
#'     instead.
#'
#'     \code{\link{unresolved}} may be used on a mirai, returning TRUE if a
#'     \sQuote{mirai} has yet to resolve and FALSE otherwise. This is suitable
#'     for use in control flow statements such as \code{while} or \code{if}.
#'
#'     Alternatively, to call (and wait for) the result, use
#'     \code{\link{call_mirai}} on the returned \sQuote{mirai}. This will block
#'     until the result is returned.
#'
#'     Specify \sQuote{.compute} to send the mirai using a specific compute
#'     profile (if previously created by \code{\link{daemons}}), otherwise leave
#'     as \sQuote{default}.
#'
#' @section Evaluation:
#'
#'     The expression \sQuote{.expr} will be evaluated in a separate R process
#'     in a clean environment (not the global environment), consisting only of
#'     the objects in the list or environment supplied to \sQuote{.args}, with
#'     the named objects passed as \sQuote{...} (from the environment if one was
#'     supplied) assigned to the global environment of that process.
#'
#'     For evaluation to occur \emph{as if} in your global environment, supply
#'     objects to \sQuote{...} rather than \sQuote{.args}. For stricter scoping,
#'     use \sQuote{.args}, which limits, for example, where variables not
#'     explicitly passed as arguments to functions are found.
#'
#'     As evaluation occurs in a clean environment, all undefined objects must
#'     be supplied though \sQuote{...} and/or \sQuote{.args}, including
#'     self-defined functions. Functions from a package should use namespaced
#'     calls such as \code{mirai::mirai()}, or else the package should be loaded
#'     beforehand as part of \sQuote{.expr}.
#'
#' @section Errors:
#'
#'     If an error occurs in evaluation, the error message is returned as a
#'     character string of class \sQuote{miraiError} and \sQuote{errorValue}
#'     (the stack trace is available at \code{$stack.trace} on the error
#'     object). \code{\link{is_mirai_error}} may be used to test for this.
#'
#'     \code{\link{is_error_value}} tests for all error conditions including
#'     \sQuote{mirai} errors, interrupts, and timeouts.
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
#' # evaluating scripts using source() in '.expr'
#' n <- 10L
#' file <- tempfile()
#' cat("r <- rnorm(n)", file = file)
#' m <- mirai({source(file); r}, file = file, n = n)
#' call_mirai(m)[["data"]]
#' unlink(file)
#'
#' # use source(local = TRUE) when passing in local variables via '.args'
#' n <- 10L
#' file <- tempfile()
#' cat("r <- rnorm(n)", file = file)
#' m <- mirai({source(file, local = TRUE); r}, .args = list(file = file, n = n))
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

  missing(.expr) && stop(._[["missing_expression"]])

  expr <- substitute(.expr)
  globals <- list(...)
  glen <- length(globals)
  if (glen) {
    gn <- names(globals)
    if (is.null(gn)) {
      glen == 1L && is.environment(globals[[1L]]) || stop(._[["named_args"]])
      globals <- as.list.environment(globals[[1L]])
    }
    all(nzchar(gn)) || stop(._[["named_args"]])
  }
  arglist <- list(
    ._mirai_globals_. = globals,
    .expr = if (is.symbol(expr) && exists(expr, where = parent.frame()) && is.language(.expr)) .expr else expr
  )
  if (length(.args))
    arglist <- c(if (is.environment(.args)) as.list.environment(.args) else .args, arglist)
  data <- list2env(arglist, envir = NULL, parent = .GlobalEnv)

  envir <- ..[[.compute]]
  if (is.null(envir)) {
    sock <- ephemeral_daemon(local_url())
    aio <- request(.context(sock), data = data,
                   send_mode = 1L, recv_mode = 1L, timeout = .timeout)
    `attr<-`(.subset2(aio, "aio"), "sock", sock)
  } else {
    aio <- request_signal(.context(envir[["sock"]]), data = data, cv = envir[["cv"]],
                          send_mode = 3L, recv_mode = 1L, timeout = .timeout)
  }

  aio

}

#' Evaluate Everywhere
#'
#' Evaluate an expression \sQuote{everywhere} on all connected daemons for the
#'     specified compute profile. Designed for performing setup operations
#'     across daemons or exporting common data, resultant changes to the global
#'     environment, loaded packages or options are persisted regardless of a
#'     daemon's \sQuote{cleanup} setting.
#'
#' @inheritParams mirai
#'
#' @return Invisible NULL.
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
#' # '...' variables are assigned to the global environment:
#' everywhere({}, a = 1, b = 2)
#' m <- mirai(a + b - y == 0L)
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

  if (length(envir)) {

    expr <- substitute(.expr)
    .expr <- c(
      as.expression(if (is.symbol(expr) && exists(expr, where = parent.frame()) && is.language(.expr)) .expr else expr),
      .snapshot
    )

    if (is.null(envir[["sockc"]])) {
      for (i in seq_len(max(stat(envir[["sock"]], "pipes"), envir[["n"]])))
        mirai(.expr = .expr, ..., .args = .args, .compute = .compute)
    } else {
      .expr <- c(.expr, .block)
      for (i in seq_len(envir[["n"]]))
        mirai(.expr = .expr, ..., .args = .args, .compute = .compute)
    }

  }

}

#' mirai (Call Value)
#'
#' \code{call_mirai} waits for the \sQuote{mirai} to resolve if still in
#'     progress, storing the value at \code{$data}.
#'
#' @param x a \sQuote{mirai} object.
#'
#' @return The passed \sQuote{mirai} (invisibly). The retrieved value is stored
#'     at \code{$data}.
#'
#' @details This function will wait for the asynchronous operation to complete
#'     if still in progress (blocking).
#'
#'     The \sQuote{mirai} updates itself in place, so to access the value of a
#'     \sQuote{mirai} \code{x} directly, use \code{call_mirai(x)$data}.
#'
#' @inheritSection mirai Errors
#'
#' @section Alternatively:
#'
#'     The value of a \sQuote{mirai} may be accessed at any time at
#'     \code{$data}, and if yet to resolve, an \sQuote{unresolved} logical NA
#'     will be returned instead.
#'
#'     Using \code{\link{unresolved}} on a \sQuote{mirai} returns TRUE only if
#'     it has yet to resolve and FALSE otherwise. This is suitable for use in
#'     control flow statements such as \code{while} or \code{if}.
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
call_mirai <- function(x) call_aio(x)

#' mirai (Call Value)
#'
#' \code{call_mirai_} is a variant that allows user interrupts, suitable for
#'     interactive use.
#'
#' @rdname call_mirai
#' @export
#'
call_mirai_ <- function(x) call_aio_(x)

#' mirai (Stop)
#'
#' Stops a \sQuote{mirai} if still in progress, causing it to resolve
#'     immediately to an \sQuote{errorValue} 20 (Operation canceled).
#'
#' @param x a \sQuote{mirai} object.
#'
#' @return Invisible NULL.
#'
#' @details Forces the \sQuote{mirai} to resolve immediately. Has no effect if
#'     the \sQuote{mirai} has already resolved.
#'
#'     If cancellation was successful, the value at \code{$data} will be an
#'     \sQuote{errorValue} 20 (Operation canceled). Note that in such a case,
#'     the \sQuote{mirai} has been aborted and the value not retrieved - but any
#'     ongoing evaluation in the daemon process will continue to completion and
#'     is not interrupted.
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
stop_mirai <- function(x) stop_aio(x)

#' Query if a mirai is Unresolved
#'
#' Query whether a \sQuote{mirai} or \sQuote{mirai} value remains unresolved.
#'     Unlike \code{\link{call_mirai}}, this function does not wait for
#'     completion.
#'
#' @param x a \sQuote{mirai} object or \sQuote{mirai} value stored at
#'     \code{$data}.
#'
#' @return Logical TRUE if \sQuote{aio} is an unresolved \sQuote{mirai} or
#'     \sQuote{mirai} value, or FALSE otherwise.
#'
#' @details Suitable for use in control flow statements such as \code{while} or
#'     \code{if}.
#'
#'     Note: querying resolution may cause a previously unresolved
#'     \sQuote{mirai} to resolve.
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
unresolved <- function(x) .unresolved(x)

.unresolved <- nanonext::unresolved

#' Is mirai
#'
#' Is the object a \sQuote{mirai}.
#'
#' @param x an object.
#'
#' @return Logical TRUE if \sQuote{x} is of class \sQuote{mirai}, FALSE
#'     otherwise.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' df <- data.frame()
#' m <- mirai(as.matrix(df), df = df)
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
#' @details Is the object a \sQuote{miraiError}. When execution in a
#'     \sQuote{mirai} process fails, the error message is returned as a
#'     character string of class \sQuote{miraiError} and \sQuote{errorValue}.
#'     The stack trace is available at \code{$stack.trace} on the error object.
#'
#'     Is the object a \sQuote{miraiInterrupt}. When an ongoing \sQuote{mirai}
#'     is sent a user interrupt, it will resolve to an empty character string
#'     classed as \sQuote{miraiInterrupt} and \sQuote{errorValue}.
#'
#'     Is the object an \sQuote{errorValue}, such as a \sQuote{mirai} timeout,
#'     a \sQuote{miraiError} or a \sQuote{miraiInterrupt}. This is a catch-all
#'     condition that includes all returned error values.
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
`[.mirai` <- function(x, i) aio_collect_(x)

#' @export
#'
print.mirai <- function(x, ...) {

  cat("< mirai | $data >\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.miraiError <- function(x, ...) {

  cat(strcat("'miraiError' chr ", x), file = stdout())
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
  attr(x, name, exact = FALSE)

#' @export
#'
.DollarNames.miraiError <- function(x, pattern = "")
  grep(pattern, "stack.trace", value = TRUE, fixed = TRUE)

# internals --------------------------------------------------------------------

ephemeral_daemon <- function(url) {
  sock <- req_socket(url, resend = 0L)
  system2(command = .command, args = c("-e", shQuote(sprintf("mirai::.daemon('%s')", url))), stdout = FALSE, stderr = FALSE, wait = FALSE)
  sock
}

deparse_safe <- function(x) if (length(x))
  deparse(x, width.cutoff = 500L, backtick = TRUE, control = NULL, nlines = 1L)

deparse_call <- function(call) {
  srcref <- attr(call, "srcref")
  if (is.null(srcref)) deparse_safe(call) else as.character(srcref)
}

mk_interrupt_error <- function() .miraiInterrupt

mk_mirai_error <- function(e, sc) {
  call <- deparse_safe(.subset2(e, "call"))
  msg <- if (is.null(call) || call == "eval(expr = ._mirai_.[[\".expr\"]], envir = ._mirai_., enclos = NULL)")
    sprintf("Error: %s", .subset2(e, "message")) else
      sprintf("Error in %s: %s", call, .subset2(e, "message"))
  cat(strcat(msg, "\n"), file = stderr())
  idx <- which(as.logical(lapply(sc, identical, quote(eval(expr = ._mirai_.[[".expr"]], envir = ._mirai_., enclos = NULL)))))
  sc <- sc[(length(sc) - 1L):(idx + 1L)]
  if (sc[[1L]][[1L]] == ".handleSimpleError")
    sc <- sc[-1L]
  sc <- lapply(sc, deparse_call)
  `class<-`(`attr<-`(msg, "stack.trace", sc), c("miraiError", "errorValue", "try-error"))
}

.miraiInterrupt <- `class<-`("", c("miraiInterrupt", "errorValue", "try-error"))
.snapshot <- expression(mirai:::snapshot())
.block <- expression(mirai:::block())
