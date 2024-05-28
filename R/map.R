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
#' Asynchronous parallel / distributed map of a function over a list or vector
#'     using \pkg{mirai}, with optional \pkg{promises} integration.
#'
#' @param .x a list or atomic vector.
#' @param .f a function to be applied to each element of \code{.x}.
#' @param ... (optional) named arguments (name = value pairs) specifying objects
#'     referenced, but not defined, in \code{.f}, or an environment containing
#'     such objects.
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
#' @return A \sQuote{mirai_map} object (list of \sQuote{mirai} objects).
#'
#' @section Results:
#'
#'     \code{x[]} collects the results of a mirai_map \code{x}. This will wait
#'     for all asynchronous operations to complete if still in progress,
#'     blocking but user-interruptible.
#'
#'     \code{x[.progress]} collects the results whilst showing a text progress
#'     indicator.
#'
#'     \code{x[.stop]} collects the results applying early stopping, which stops
#'     at the first failure and aborts all remaining in-progress operations.
#'
#'     \code{x[c(.stop, .progress)]} combines early stopping with a progress
#'     indicator.
#'
#' @details Sends each application of function \code{.f} on an element of
#'     \code{.x} for computation in a separate \code{\link{mirai}} call.
#'
#'     This simple and transparent behaviour is designed to make full use of
#'     \pkg{mirai} scheduling to minimise overall execution time.
#'
#'     Facilitates recovery from partial failure by returning all
#'     \sQuote{miraiError} / \sQuote{errorValue} as the case may be, thus
#'     allowing only the failures to be re-run. If using dispatcher,
#'     \sQuote{retry} should be specified as FALSE to ensure crashes are
#'     returned as errors.
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
#' res <- mirai_map(1:3, rnorm, .args = list(mean = 20, sd = 2))[]
#' res
#'
#' mp <- mirai_map(
#'   c(a = 2, b = 3, c = 4),
#'   function(x) do(x, as.logical(x %% 2)),
#'   do = nanonext::random
#' )
#' mp
#' mp[]
#'
#' # progress indicator counts up to 4 seconds
#' res <- mirai_map(1:4, Sys.sleep)[.progress]
#'
#' daemons(0)
#'
#' # generates warning as daemons not set
#' # stops early when second element returns an error
#' tryCatch(
#'   mirai_map(list(a = 1, b = "a", c = 3), sum)[.stop],
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
    warning(._[["requires_daemons"]], immediate. = TRUE)
    daemons(n = 1L, dispatcher = FALSE, retry = FALSE, .compute = .compute)
    return(mirai_map(.x = .x, .f = .f, ..., .args = .args, .promise = .promise, .compute = .compute))
  }
  vec <- vector(mode = "list", length = length(.x))
  for (i in seq_along(vec))
    vec[[i]] <- mirai(
      .expr = do.call(.f, c(list(.x), .args)),
      .f = .f,
      .x = .subset2(.x, i),
      ...,
      .args = list(.args = .args),
      .compute = .compute
    )

  if (length(.promise)) {
    if (is.list(.promise)) {
      if (length(.promise) > 1L)
        lapply(vec, promises::then, .promise[[1L]], .promise[[2L]]) else
          lapply(vec, promises::then, .promise[[1L]])
    } else {
      lapply(vec, promises::then, .promise)
    }
  }

  `class<-`(`names<-`(vec, names(.x)), "mirai_map")

}

#' @export
#'
`[.mirai_map` <- function(x, i) {

  missing(i) && return(collect_aio_(x))

  .expr <- i
  xi <- i <- 0L
  xlen <- length(x)
  eval(.expr)
  for (i in seq_len(xlen)) {
    xi <- collect_aio_(x[[i]])
    eval(.expr)
  }

  lapply(x, .subset2, "value")

}

#' @export
#'
print.mirai_map <- function(x, ...) {

  cat(sprintf("< mirai map | %d items >\n", length(x)), file = stdout())
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
.progress <- expression(cat(if (i < xlen) sprintf("\r[ %d / %d .... ]", i, xlen) else sprintf("\r[ %d / %d done ]\n", i, xlen), file = stderr()))

#' @rdname dot-progress
#' @export
#'
.stop <- expression(if (is_error_value(xi)) { lapply(x, stop_mirai); stop(xi, call. = FALSE) })
