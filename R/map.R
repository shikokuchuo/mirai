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

#' mirai Map Functions
#'
#' \code{mmap} maps a function over a list or vector using \pkg{mirai}, waiting
#'     for completion.
#'
#' @param .x a list or atomic vector.
#' @param .f a function to be applied to each element of \code{.x}.
#' @param ... optional arguments to \code{.f}.
#' @param .args optional arguments to \code{.f} provided as a list.
#' @param .progress [default FALSE] if TRUE, reports progress via a simple text
#'     progress indicator to \sQuote{stderr}.
#' @param .stop [default FALSE] errors are returned as \sQuote{miraiError} /
#'     \sQuote{errorValue} as the case may be, allowing recovery from partial
#'     failure. If TRUE, performs early stopping as soon as an error is
#'     encountered, with remaining computations aborted.
#' @inheritParams mirai
#'
#' @return A list, the same length as \code{.x}, preserving names.
#'
#' @details These functions send each application of function \code{.f} on an
#'     element of \code{.x} for computation in a separate \code{\link{mirai}}
#'     call.
#'
#'     Chunking (processing multiple elements of \code{.x} in batches) is not
#'     performed. This approach is particularly suited to lengthy computations
#'     or those for which compute times over the indices are variable or
#'     unpredictable, where it can be optimal to rely on \pkg{mirai} scheduling.
#'
#'     Note: daemons are assumed to have been previously set, otherwise new
#'     ephemeral daemons will be created for each computation.
#'
#' @section mmap:
#'
#'     Offers the option to show a simple text progress indicator.
#'
#'     Designed to facilitate recovery from partial failure by returning all
#'     \sQuote{miraiError} / \sQuote{errorValue} as the case may be by default,
#'     thus allowing only the failures to be re-run.
#'
#'     Alternatively, there is the option for early stopping, which stops at the
#'     first failure and aborts all remaining computations.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' with(
#'   daemons(3, dispatcher = FALSE),
#'   mmap(1:3, rnorm, mean = 20, .args = list(sd = 2))
#' )
#'
#' # progress indicator counts up to 4 seconds
#' with(daemons(4, dispatcher = FALSE), mmap(1:4, Sys.sleep, .progress = TRUE))
#'
#' # creates 3 ephemeral daemons as daemons not set
#' # second element returns a 'miraiError'
#' mmap(list(a = 1, b = "a", c = 3), sum)
#'
#' ml <- mwalk(c(a = 2, b = 3, c = 4), rnorm, mean = 20, .args = list(sd = 2))
#' ml
#' collect_mirai(ml)
#'
#' }
#'
#' @export
#'
mmap <- function(.x, .f, ..., .args = list(), .progress = FALSE, .stop = FALSE, .compute = "default") {

  .progress || .stop ||
    return(collect_aio_(mwalk(.x = .x, .f = .f, ..., .args = .args, .compute = .compute)))

  xlen <- length(.x)
  vec <- vector(mode = "list", length = xlen)
  for (i in seq_len(xlen))
    vec[[i]] <- mirai(
      .expr = do.call(.f, c(list(.x), .args)),
      .args = list(.f = .f, .x = .subset2(.x, i), .args = c(list(...), .args)),
      .compute = .compute
    )
  for (i in seq_len(xlen)) {
    if (.progress)
      cat(sprintf("\r[ %d / %d .... ]", i - 1L, xlen), file = stderr())
    res <- collect_aio_(vec[[i]])
    .stop && is_error_value(res) && {
      lapply(vec, stop_aio)
      stop(res)
    }
  }
  if (.progress)
    cat(sprintf("\r[ %d / %d done ]\n", xlen, xlen), file = stderr())

  `names<-`(lapply(vec, .subset2, "value"), names(.x))

}

#' mirai Walk
#'
#' \code{mwalk} walks a function over a list or vector using \pkg{mirai} for the
#'     side-effects, not waiting for completion.
#'
#' @section mwalk:
#'
#'     Whilst this function is designed primarily to enact side effects, it may
#'     also be used as an asynchronous map function by returning a list of
#'     \sQuote{mirai} objects. Their values may be subsequently collected using
#'     \code{\link{collect_mirai}}.
#'
#' @rdname mmap
#' @export
#'
mwalk <- function(.x, .f, ..., .args = list(), .compute = "default") {

  vec <- vector(mode = "list", length = length(.x))
  for (i in seq_along(vec))
    vec[[i]] <- mirai(
      .expr = do.call(.f, c(list(.x), .args)),
      .args = list(.f = .f, .x = .subset2(.x, i), .args = c(list(...), .args)),
      .compute = .compute
    )
  `names<-`(vec, names(.x))

}
