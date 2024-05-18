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

#' mirai Map / Collect
#'
#' \code{mmap} is an asynchronous parallel / distributed map function. Maps a
#'     function over a list or vector, returning a list of \sQuote{mirai}
#'     objects.
#'
#' @param .x a list or atomic vector.
#' @param .f a function to be applied to each element of \code{.x}.
#' @param ... optional constant arguments to \code{.f}.
#' @param .args optional constant arguments to \code{.f}, provided as a list.
#' @inheritParams mirai
#'
#' @return For \code{mmap}: a list of \sQuote{mirai} objects, the same length as
#'     \code{.x}, preserving names.
#'
#' @section mmap:
#'
#'     Sends each application of function \code{.f} on an element of \code{.x}
#'     for computation in a separate \code{\link{mirai}} call.
#'
#'     This simple and transparent behaviour is designed to make full use of
#'     \pkg{mirai} scheduling to optimise overall execution time.
#'
#'     Facilitates recovery from partial failure by returning all
#'     \sQuote{miraiError} / \sQuote{errorValue} as the case may be, thus
#'     allowing only the failures to be re-run.
#'
#'     Note: daemons are assumed to have been previously set, otherwise new
#'     ephemeral daemons will be created for each computation.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' with(
#'   daemons(3, dispatcher = FALSE),
#'   mcollect(mmap(1:3, rnorm, mean = 20, .args = list(sd = 2)))
#' )
#'
#' # progress indicator counts up to 4 seconds
#' with(
#'   daemons(4, dispatcher = FALSE),
#'   mcollect(mmap(1:4, Sys.sleep), progress = TRUE)
#' )
#'
#' # creates 3 ephemeral daemons as daemons not set
#' # second element returns a 'miraiError'
#' mcollect(mmap(list(a = 1, b = "a", c = 3), sum))
#'
#' ml <- mmap(c(a = 2, b = 3, c = 4), rnorm, mean = 20, .args = list(sd = 2))
#' ml
#'
#' mcollect(ml)
#'
#' }
#'
#' @export
#'
mmap <- function(.x, .f, ..., .args = list(), .compute = "default") {

  vec <- vector(mode = "list", length = length(.x))
  for (i in seq_along(vec))
    vec[[i]] <- mirai(
      .expr = do.call(.f, c(list(.x), .args)),
      .args = list(.f = .f, .x = .subset2(.x, i), .args = c(list(...), .args)),
      .compute = .compute
    )
  `names<-`(vec, names(.x))

}

#' mirai Map / Collect
#'
#' \code{mcollect} collects the results from \code{mmap} or any list of
#'     \sQuote{mirai} objects, waiting for resolution if still in progress.
#'
#' @param x a list of \sQuote{mirai} objects.
#' @param progress [default FALSE] if TRUE, reports progress via a simple text
#'     progress indicator.
#' @param stop [default FALSE] errors are returned as \sQuote{miraiError} /
#'     \sQuote{errorValue} as the case may be, allowing recovery from partial
#'     failure. If TRUE, performs early stopping as soon as an error is
#'     encountered, with remaining in-progress computations aborted.
#'
#' @return For \code{mcollect}: a list, the same length as \sQuote{x},
#'     preserving names.
#'
#' @section mcollect:
#'
#'     This function will wait for all asynchronous operation(s) to
#'     complete if still in progress (blocking, although user-interruptible).
#'
#'     Optionally shows a simple text progress indicator.
#'
#'     Allows for early stopping, which stops at the first failure and aborts
#'     all remaining in-progress computations.
#'
#' @rdname mmap
#' @export
#'
mcollect <- function(x, progress = FALSE, stop = FALSE) {

  progress || stop || return(aio_collect_(x))

  xlen <- length(x)
  for (i in seq_len(xlen)) {
    if (progress)
      cat(sprintf("\r[ %d / %d .... ]", i - 1L, xlen), file = stderr())
    res <- aio_collect_(x[[i]])
    stop && is_error_value(res) && {
      lapply(x, stop_aio)
      stop(res)
    }
  }
  if (progress)
    cat(sprintf("\r[ %d / %d done ]\n", xlen, xlen), file = stderr())

  lapply(x, .subset2, "value")

}
