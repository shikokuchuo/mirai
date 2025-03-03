# Copyright (C) 2024-2025 Hibiki AI Limited <info@hibiki-ai.com>
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
#' \pkg{mirai}, with optional \pkg{promises} integration. Performs multiple map
#' over the rows of a dataframe or matrix.
#'
#' Sends each application of function `.f` on an element of `.x` (or row of
#' `.x`) for computation in a separate [mirai()] call. If `.x` is named, names
#' are preserved.
#'
#' This simple and transparent behaviour is designed to make full use of
#' \pkg{mirai} scheduling to minimise overall execution time.
#'
#' Facilitates recovery from partial failure by returning all
#' \sQuote{miraiError} / \sQuote{errorValue} as the case may be, thus allowing
#' only the failures to be re-run.
#'
#' This function requires daemons to have previously been set, and will error if
#' not.
#'
#' @param .x a list or atomic vector. Also accepts a matrix or dataframe, in
#'   which case multiple map is performed over its rows.
#' @param .f a function to be applied to each element of `.x`, or row of `.x` as
#'   the case may be.
#' @param ... (optional) named arguments (name = value pairs) specifying objects
#'   referenced, but not defined, in `.f`.
#' @param .args (optional) further constant arguments to `.f`, provided as a
#'   list.
#' @param .promise (optional) if supplied, registers a promise against each
#'   mirai. Either a function, supplied to the `onFulfilled` argument of
#'   `promises::then()` or a list of 2 functions, supplied respectively to
#'   `onFulfilled` and `onRejected` of `promises::then()`. Using this argument
#'   requires the \CRANpkg{promises} package.
#' @inheritParams mirai
#'
#' @return A \sQuote{mirai_map} (list of \sQuote{mirai} objects).
#'
#' @section Collection Options:
#'
#' `x[]` collects the results of a \sQuote{mirai_map} `x` and returns
#' a list. This will wait for all asynchronous operations to complete if still
#' in progress, blocking but user-interruptible.
#'
#' `x[.flat]` collects and flattens map results to a vector, checking that
#' they are of the same type to avoid coercion. Note: errors if an
#' \sQuote{errorValue} has been returned or results are of differing type.
#'
#' `x[.progress]` collects map results whilst showing a progress bar from
#' the \CRANpkg{cli} package, if installed, with completion percentage and ETA,
#' or else a simple text progress indicator. Note: if the map operation
#' completes too quickly then the progress bar may not show at all.
#'
#' `x[.stop]` collects map results applying early stopping, which stops at
#' the first failure and cancels remaining operations. Note: operations already
#' in progress continue to completion, although their results are not collected.
#'
#' The options above may be combined in the manner of: \cr
#' `x[.stop, .progress]` which applies early stopping together with a
#' progress indicator.
#'
#' @section Multiple Map:
#'
#' If `.x` is a matrix or dataframe (or other object with \sQuote{dim}
#' attributes), *multiple* map is performed over its **rows**. Character row
#' names are preserved as names of the output.
#'
#' This allows map over 2 or more arguments, and `.f` should accept at least as
#' many arguments as there are columns. If the dataframe has names, or the
#' matrix column dimnames, named arguments are provided to `.f`.
#'
#' To map over **columns** instead, first wrap a dataframe in [as.list()], or
#' transpose a matrix using [t()].
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' daemons(4)
#'
#' # perform and collect mirai map
#' mm <- mirai_map(c(a = 1, b = 2, c = 3), rnorm)
#' mm
#' mm[]
#'
#' # map with constant args specified via '.args'
#' mirai_map(1:3, rnorm, .args = list(n = 5, sd = 2))[]
#'
#' # flatmap with helper function passed via '...'
#' mirai_map(
#'   10^(0:9),
#'   function(x) rnorm(1L, valid(x)),
#'   valid = function(x) min(max(x, 0L), 100L)
#' )[.flat]
#'
#' # unnamed matrix multiple map: arguments passed to function by position
#' (mat <- matrix(1:4, nrow = 2L))
#' mirai_map(mat, function(x = 10, y = 0, z = 0) x + y + z)[.flat]
#'
#' # named matrix multiple map: arguments passed to function by name
#' dimnames(mat) <- list(c("a", "b"), c("y", "z"))
#' mirai_map(mat, function(x = 10, y = 0, z = 0) x + y + z)[.flat]
#'
#' # dataframe multiple map: using a function taking '...' arguments
#' df <- data.frame(a = c("Aa", "Bb"), b = c(1L, 4L))
#' mirai_map(df, function(...) sprintf("%s: %d", ...))[.flat]
#'
#' # indexed map over a vector (using a dataframe)
#' v <- c("egg", "got", "ten", "nap", "pie")
#' mirai_map(
#'   data.frame(1:length(v), v),
#'   sprintf,
#'   .args = list(fmt = "%d_%s")
#' )[.flat]
#'
#' # return a 'mirai_map' object, check for resolution, collect later
#' mp <- mirai_map(2:4, function(x) runif(1L, x, x + 1))
#' unresolved(mp)
#' mp
#' mp[.flat]
#' unresolved(mp)
#'
#' # progress indicator counts up from 0 to 4 seconds
#' res <- mirai_map(1:4, Sys.sleep)[.progress]
#'
#' # stops early when second element returns an error
#' tryCatch(mirai_map(list(1, "a", 3), sum)[.stop], error = identity)
#'
#' daemons(0)
#'
#' # promises example that outputs the results, including errors, to the console
#' if (requireNamespace("promises", quietly = TRUE)) {
#' daemons(1, dispatcher = FALSE)
#' ml <- mirai_map(
#'   1:30,
#'   function(i) {Sys.sleep(0.1); if (i == 30) stop(i) else i},
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

  envir <- ..[[if (missing(.compute)) .[["cp"]] else .compute]]
  is.null(envir) && stop(._[["requires_daemons"]])
  is.function(.f) || stop(sprintf(._[["function_required"]], typeof(.f)))

  dx <- dim(.x)
  vec <- if (is.null(dx)) {
    `names<-`(
      lapply(
        .x,
        function(x)
          mirai(
            .expr = do.call(.f, c(list(.x), .args), quote = TRUE),
            ...,
            .args = list(.f = .f, .x = x, .args = .args),
            .compute = .compute
          )
      ),
      names(.x)
    )
  } else {
    if (is.matrix(.x)) {
      `names<-`(
        lapply(
          seq_len(dx[1L]),
          function(i)
            mirai(
              .expr = do.call(.f, c(as.vector(.x, mode = "list"), .args), quote = TRUE),
              ...,
              .args = list(.f = .f, .x = .x[i, ], .args = .args),
              .compute = .compute
            )
        ),
        dimnames(.x)[[1L]]
      )
    } else {
      rn <- attr(.x, "row.names")
      `names<-`(
        lapply(
          seq_len(dx[1L]),
          function(i)
            mirai(
              .expr = do.call(.f, c(.x, .args), quote = TRUE),
              ...,
              .args = list(.f = .f, .x = lapply(.x, `[[`, i), .args = .args),
              .compute = .compute
            )
        ),
        if (is.character(rn)) rn
      )
    }
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

  ensure_cli_initialized()
  dots <- eval(`[[<-`(substitute(alist(...)), 1L, quote(list)), envir = .)
  map(x, dots)

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
#' Expressions to be provided to the `[]` method for \sQuote{mirai_map} objects.
#'
#' @inheritSection mirai_map Collection Options
#'
#' @keywords internal
#' @export
#'
.flat <- compiler::compile(
  quote(
    if (i == 0L) xi <- TRUE else
      if (i == 1L) typ <<- typeof(xi) else
        if (i <= xlen) {
          is_error_value(xi) && {
            stop_mirai(x)
            stop(sprintf("In index %d:\n%s", i, attr(xi, "message")), call. = FALSE)
          }
          typeof(xi) != typ && {
            stop_mirai(x)
            stop(sprintf("Cannot flatten outputs of differing type: %s / %s", typ, typeof(xi)), call. = FALSE)
          }
        }
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
.stop <- compiler::compile(
  quote(if (is_error_value(xi)) { stop_mirai(x); stop(sprintf("In index %d:\n%s", i, attr(xi, "message")), call. = FALSE) })
)

# internals --------------------------------------------------------------------

ensure_cli_initialized <- function()
  if (is.null(.[[".flat"]])) {
    cli <- requireNamespace("cli", quietly = TRUE)
    `[[<-`(`[[<-`(`[[<-`(., ".flat", if (cli) flat_cli else .flat), ".progress", if (cli) progress_cli else .progress), ".stop", if (cli) stop_cli else .stop)
  }

map <- function(x, dots) {

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

flat_cli <- compiler::compile(
  quote(
    if (i == 0L) xi <- TRUE else
      if (i == 1L) typ <<- typeof(xi) else
        if (i <= xlen) {
          is_error_value(xi) && {
            stop_mirai(x)
            iname <- names(x)[i]
            cli::cli_abort(
              c(i = "In index: {i}.",
                i = if (length(iname) && nzchar(iname)) "With name: {iname}."),
              location = i,
              name = iname,
              parent = `class<-`(attributes(xi), c("error", "condition")),
              call = quote(mirai_map())
            )
          }
          typeof(xi) != typ && {
            stop_mirai(x)
            iname <- names(x)[i]
            cli::cli_abort(
              c(`!` = "cannot flatten outputs of differing type: {typ} / {typeof(xi)}"),
              location = i,
              name = iname,
              call = quote(mirai_map())
            )
          }
        }

  )
)

progress_cli <- compiler::compile(
  quote(
    if (i == 0L) cli::cli_progress_bar(type = NULL, total = xlen, auto_terminate = TRUE, .envir = .) else
      if (i <= xlen) cli::cli_progress_update(.envir = .)
  )
)

stop_cli <- compiler::compile(
  quote(
    if (is_error_value(xi)) {
      stop_mirai(x)
      iname <- names(x)[i]
      cli::cli_abort(
        c(i = "In index: {i}.",
          i = if (length(iname) && nzchar(iname)) "With name: {iname}."),
        location = i,
        name = iname,
        parent = `class<-`(attributes(xi), c("error", "condition")),
        call = quote(mirai_map())
      )
    }
  )
)
