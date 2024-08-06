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
#'     map over 2D lists/vectors, allowing advanced patterns such as map over
#'     the rows of a dataframe or matrix.
#'
#' @param .x a list or atomic vector. If a 2D list (i.e. list of lists/vectors
#'     of the same length), multiple map is performed over a slice of the list.
#'     If a matrix, multiple map is performed over the rows of the matrix.
#' @param .f a function to be applied to each element of \code{.x}, or across
#'     all sub-elements of \code{.x} as the case may be.
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
#'     \code{x[.progress]} collects map results whilst showing a text progress
#'     indicator.
#'
#'     \code{x[.stop]} collects map results applying early stopping, which stops
#'     at the first failure and cancels remaining operations. Note: operations
#'     already in progress continue to completion, although their results are
#'     not collected.
#'
#'     The options above may be combined in a vector, for example: \cr
#'     \code{x[c(.stop, .progress)]} applies early stopping together with a
#'     progress indicator.
#'
#' @details Sends each application of function \code{.f} on an element of
#'     \code{.x} (or each application of \code{.f} on a slice of \code{.x}) for
#'     computation in a separate \code{\link{mirai}} call.
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
#' daemons(4, dispatcher = FALSE)
#'
#' # map with constant args specified via '.args'
#' mirai_map(1:3, rnorm, .args = list(mean = 20, sd = 2))[]
#'
#' # flatmap with function definition passed via '...'
#' mirai_map(1:3, function(x) func(1L, x, x + 1L), func = stats::runif)[.flat]
#'
#' # sum slices of a list of vectors
#' (listvec <- list(1:3, c(4, 3, 2)))
#' mirai_map(listvec, sum)[.flat]
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
#' mirai_map(list(1:length(v), v), sprintf, .args = list(fmt = "%d_%s"))[.flat]
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
#'   mirai_map(list(list(a = 1, b = "a", c = 3), 3:1), sum)[.stop],
#'   error = identity
#' )
#'
#' # promises example that outputs the results, including errors, to the console
#' if (requireNamespace("promises", quietly = TRUE)) {
#' daemons(1, dispatcher = FALSE)
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

  envir <- ..[[.compute]]
  is.null(envir) && {
    .x
    .f
    warning(._[["requires_daemons"]], call. = FALSE, immediate. = TRUE)
    daemons(n = 1L, dispatcher = FALSE, .compute = .compute)
    return(mirai_map(.x = .x, .f = .f, ..., .args = .args, .promise = .promise, .compute = .compute))
  }
  if (is.matrix(.x)) {
    xilen <- length(.x[1L, ])
    cond <- xilen > 1L
  } else {
    xilen <- length(.x[[1L]])
    cond <- xilen > 1L && all(as.integer(lapply(.x, length)) == xilen)
  }
  if (cond) {
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
`[.mirai_map` <- function(x, i) {

  missing(i) && return(collect_aio_(x))

  .expr <- i
  xi <- i <- 0L
  xlen <- length(x)
  out <- `names<-`(vector(mode = "list", length = xlen), names(x))
  eval(.expr)
  for (i in seq_len(xlen)) {
    xi <- collect_aio_(x[[i]])
    if (!is.null(xi)) out[[i]] <- xi
    eval(.expr)
  }
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
.flat <- expression({ if (i <= 1L) typ <- typeof(xi) else is_error_value(xi) && stop(xi, call. = FALSE) || typeof(xi) == typ || stop(sprintf("cannot flatten outputs of differing type: %s / %s", typ, typeof(xi)), call. = FALSE); if (i == xlen) out <- unlist(out, recursive = FALSE) })

#' @rdname dot-flat
#' @export
#'
.progress <- expression(cat(if (i < xlen) sprintf("\r[ %d / %d .... ]", i, xlen) else sprintf("\r[ %d / %d done ]\n", i, xlen), file = stderr()))

#' @rdname dot-flat
#' @export
#'
.stop <- expression(if (is_error_value(xi)) { lapply(x, stop_mirai); stop(xi, call. = FALSE) })
