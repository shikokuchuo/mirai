# Copyright (C) 2024 Hibiki AI Limited <info@hibiki-ai.com>
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

# mirai map functions ----------------------------------------------------------

#' mirai Map
#'
#' Asynchronous parallel map of a function over a list or vector using
#'     \pkg{mirai}, with optional \pkg{promises} integration. Performs multiple
#'     map over the rows of a dataframe or matrix.
#'
#' @param .x a list or atomic vector. Also accepts a matrix or dataframe, in
#'     which case multiple map is performed over its rows.
#' @param .f a function to be applied to each element of \code{.x}, or row of
#'     \code{.x} as the case may be.
#' @param ... (optional) named arguments (name = value pairs) specifying objects
#'     referenced, but not defined, in \code{.f}.
#' @param .args (optional) further constant arguments to \code{.f}, provided as
#'     a list.
#' @param .promise (optional) if supplied, registers a promise against each
#'     mirai. Either a function, supplied to the \sQuote{onFulfilled} argument
#'     of \code{promises::then()} or a list of 2 functions, supplied
#'     respectively to \sQuote{onFulfilled} and \sQuote{onRejected} for
#'     \code{promises::then()}. Using this argument requires the
#'     \CRANpkg{promises} package.
#' @inheritParams mirai
#'
#' @return A \sQuote{mirai_map} (list of \sQuote{mirai} objects).
#'
#' @section Results:
#'
#'     \code{x[]} collects the results of a \sQuote{mirai_map} \code{x} and
#'     returns a list. This will wait for all asynchronous operations to
#'     complete if still in progress, blocking but user-interruptible.
#'
#'     \code{x[.flat]} collects and flattens map results to a vector, checking
#'     that they are of the same type to avoid coercion. Note: errors if an
#'     \sQuote{errorValue} has been returned or results are of differing type.
#'
#'     \code{x[.progress]} collects map results whilst showing a simple text
#'     progress indicator of parts completed of the total.
#'
#'     \code{x[.progress_cli]} collects map results whilst showing a progress bar
#'     from the \CRANpkg{cli} package, if available, with completion percentage
#'     and ETA.
#'
#'     \code{x[.stop]} collects map results applying early stopping, which stops
#'     at the first failure and cancels remaining operations. Note: operations
#'     already in progress continue to completion, although their results are
#'     not collected.
#'
#'     The options above may be combined in the manner of: \cr
#'     \code{x[.stop, .progress]} which applies early stopping together with a
#'     progress indicator.
#'
#' @section Multiple Map:
#'
#'    Multiple map is performed automatically over the \strong{rows} of an
#'    object with \sQuote{dim} attributes such as a matrix or dataframe. This is
#'    most often the desired behaviour.
#'
#'    To map over \strong{columns} instead, first wrap a dataframe in
#'    \code{\link{as.list}}, or transpose a matrix using \code{\link{t}}.
#'
#' @details Sends each application of function \code{.f} on an element of
#'     \code{.x} (or row of \code{.x}) for computation in a separate
#'     \code{\link{mirai}} call.
#'
#'     This simple and transparent behaviour is designed to make full use of
#'     \pkg{mirai} scheduling to minimise overall execution time.
#'
#'     Facilitates recovery from partial failure by returning all
#'     \sQuote{miraiError} / \sQuote{errorValue} as the case may be, thus
#'     allowing only the failures to be re-run.
#'
#'     Note: requires daemons to have previously been set. If not, then one
#'     local daemon is set before the function propceeds.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' daemons(4, dispatcher = "none")
#'
#' # map with constant args specified via '.args'
#' mirai_map(1:3, rnorm, .args = list(mean = 20, sd = 2))[]
#'
#' # flatmap with function definition passed via '...'
#' mirai_map(1:3, function(x) func(1L, x, x + 1L), func = stats::runif)[.flat]
#'
#' # sum rows of a dataframe
#' (df <- data.frame(a = 1:3, b = c(4, 3, 2)))
#' mirai_map(df, sum)[.flat]
#'
#' # sum rows of a matrix
#' (mat <- matrix(1:4, nrow = 2L))
#' mirai_map(mat, sum)[.flat]
#'
#' # map over rows of a dataframe
#' df <- data.frame(a = c("Aa", "Bb"), b = c(1L, 4L))
#' mirai_map(df, function(...) sprintf("%s: %d", ...))[.flat]
#'
#' # indexed map over a vector
#' v <- c("egg", "got", "ten", "nap", "pie")
#' mirai_map(
#'   data.frame(1:length(v), v),
#'   sprintf,
#'   .args = list(fmt = "%d_%s")
#' )[.flat]
#'
#' # return a 'mirai_map' object, check for resolution, collect later
#' mp <- mirai_map(
#'   c(a = 2, b = 3, c = 4),
#'   function(x, y) do(x, as.logical(x %% y)),
#'   do = nanonext::random,
#'   .args = list(y = 2)
#' )
#' unresolved(mp)
#' mp
#' mp[]
#' unresolved(mp)
#'
#' # progress indicator counts up from 0 to 4 seconds
#' res <- mirai_map(1:4, Sys.sleep)[.progress]
#'
#' daemons(0)
#'
#' # generates warning as daemons not set
#' # stops early when second element returns an error
#' tryCatch(
#'   mirai_map(list(1, "a", 3), sum)[.stop],
#'   error = identity
#' )
#'
#' # promises example that outputs the results, including errors, to the console
#' if (requireNamespace("promises", quietly = TRUE)) {
#' daemons(1, dispatcher = "none")
#' ml <- mirai_map(
#'   1:30,
#'   function(x) {Sys.sleep(0.1); if (x == 30) stop(x) else x},
#'   .promise = list(
#'     function(x) cat(paste(x, "")),
#'     function(x) { cat(conditionMessage(x), "\n"); daemons(0) }
#'   )
#' )
#' }
#'
#' }
#'
#' @export
#'
mirai_map <- function(.x, .f, ..., .args = list(), .promise = NULL, .compute = "default") {

  is.function(.f) || stop(sprintf(._[["function_required"]], typeof(.f)))
  envir <- ..[[.compute]]
  is.null(envir) && {
    .x
    .f
    warning(._[["requires_daemons"]], call. = FALSE, immediate. = TRUE)
    daemons(n = 1L, dispatcher = "none", .compute = .compute)
    return(mirai_map(.x = .x, .f = .f, ..., .args = .args, .promise = .promise, .compute = .compute))
  }
  xilen <- dim(.x)[1L]
  if (length(xilen)) {
    vec <- vector(mode = "list", length = xilen)
    if (is.matrix(.x)) {
      for (i in seq_len(xilen))
        vec[[i]] <- mirai(
          .expr = do.call(.f, c(as.list(.x), .args)),
          .f = .f,
          .x = .x[i, ],
          ...,
          .args = list(.args = .args),
          .compute = .compute
        )
    } else {
      for (i in seq_len(xilen))
        vec[[i]] <- mirai(
        .expr = do.call(.f, c(.x, .args)),
        .f = .f,
        .x = lapply(.x, .subset2, i),
        ...,
        .args = list(.args = .args),
        .compute = .compute
      )
    }
  } else {
    vec <- `names<-`(vector(mode = "list", length = length(.x)), names(.x))
    for (i in seq_along(vec))
      vec[[i]] <- mirai(
        .expr = do.call(.f, c(list(.x), .args)),
        .f = .f,
        .x = .subset2(.x, i),
        ...,
        .args = list(.args = .args),
        .compute = .compute
      )
  }

  if (length(.promise))
    if (is.list(.promise)) {
      if (length(.promise) > 1L)
        lapply(vec, promises::then, .promise[[1L]], .promise[[2L]]) else
          lapply(vec, promises::then, .promise[[1L]])
    } else {
      lapply(vec, promises::then, .promise)
    }

  `class<-`(vec, "mirai_map")

}

#' @export
#'
`[.mirai_map` <- function(x, ...) {

  missing(..1) && return(collect_aio_(x))

  dots <- eval(`[[<-`(substitute(alist(...)), 1L, quote(list)), envir = .)
  expr <- if (length(dots) > 1L) do.call(expression, dots) else dots[[1L]]
  xlen <- length(x)
  i <- 0L
  typ <- xi <- FALSE
  collect_map <- function(i) {
    xi <- collect_aio_(x[[i]])
    eval(expr)
    xi
  }
  eval(expr)
  out <- `names<-`(lapply(seq_len(xlen), collect_map), names(x))
  xi && return(unlist(out, recursive = FALSE))
  out

}

#' @export
#'
print.mirai_map <- function(x, ...) {

  xlen <- length(x)
  cat(sprintf("< mirai map [%d/%d] >\n", xlen - .unresolved(x), xlen), file = stdout())
  invisible(x)

}

#' mirai Map Options
#'
#' Expressions to insert into the \code{[]} method for \sQuote{mirai_map}
#'     objects.
#'
#' @inheritSection mirai_map Results
#'
#' @keywords internal
#' @export
#'
.flat <- compiler::compile(
  quote(
    if (i == 0L) xi <- TRUE else
      if (i == 1L) typ <<- typeof(xi) else
        if (i <= xlen) is_error_value(xi) && stop(xi, call. = FALSE) || typeof(xi) == typ || stop(sprintf("[.flat]: cannot flatten outputs of differing type: %s / %s", typ, typeof(xi)), call. = FALSE)
  )
)

#' @rdname dot-flat
#' @export
#'
.progress <- compiler::compile(
  quote(
    if (i == 0L) cat(sprintf("\r[ 0 / %d .... ]", xlen), file = stderr()) else
      if (i < xlen) cat(sprintf("\r[ %d / %d .... ]", i, xlen), file = stderr()) else
        if (i == xlen) cat(sprintf("\r[ %d / %d done ]\n", i, xlen), file = stderr())
  )
)

#' @rdname dot-flat
#' @export
#'
.progress_cli <- compiler::compile(
  quote(
    if (i == 0L) cli::cli_progress_bar(type = NULL, total = xlen, auto_terminate = TRUE, .envir = .) else
      if (i <= xlen) cli::cli_progress_update(.envir = .)
  )
)

#' @rdname dot-flat
#' @export
#'
.stop <- compiler::compile(
  quote(if (is_error_value(xi)) { lapply(x, stop_mirai); stop(xi, call. = FALSE) })
)
