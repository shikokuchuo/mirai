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
#' \code{mmap} maps a function over a list or vector using \pkg{mirai}.
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
#' @return For \code{mmap} and \code{mwalk}: a list, the same length as
#'     \code{.x}, preserving names.
#'
#' @section mmap:
#'
#'     This function sends each application of \code{.f} on an element of
#'     \code{.x} for computation in a separate \code{\link{mirai}} call, and
#'     waits for completion.
#'
#'     Designed to facilitate recovery from partial failure by returning all
#'     results by default, allowing only the failures to be re-run.
#'
#'     Alternatively, there is the option for early stopping, which stops at the
#'     first failure and aborts all remaining computations.
#'
#'     This function does not perform chunking (assigning multiple elements of
#'     \code{.x} in batches), and is hence particularly suited to lengthy
#'     computations or those which have variable or unpredicatable compute times
#'     over the indices, where it can be optimal to rely on \pkg{mirai}
#'     scheduling.
#'
#'     Note: daemons should generally be set prior to calling this function,
#'     otherwise ephemeral daemons will be created for each computation.
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
#' mcollect(ml)
#'
#' }
#'
#' @export
#'
mmap <- function(.x, .f, ..., .args = list(), .progress = FALSE, .stop = FALSE, .compute = "default") {

  .progress || .stop ||
    return(mcollect(mwalk(.x = .x, .f = .f, ..., .args = .args, .compute = .compute)))

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
    res <- call_mirai_(vec[[i]])
    .stop && is_error_value(.subset2(res, "value")) && {
      lapply(vec, stop_aio)
      stop(.subset2(res, "value"))
    }
  }
  if (.progress)
    cat(sprintf("\r[ %d / %d done ]\n", xlen, xlen), file = stderr())

  `names<-`(lapply(vec, .subset2, "value"), names(.x))

}

#' mirai Walk
#'
#' \code{mwalk} walk a function over a list or vector using \pkg{mirai} for the
#'     side-effects.
#'
#' @section mwalk:
#'
#'     This function sends each application of \code{.f} on an element of
#'     \code{.x} for computation in a separate \code{\link{mirai}} call. It does
#'     not wait for completion.
#'
#'     Whilst this function is designed primarily to enact side effects, it may
#'     also be used as an asynchronous map function by returning a list of
#'     \sQuote{mirai}, and their values may be collected using \code{mcollect}.
#'
#'     This function does not perform chunking (assigning multiple elements of
#'     \code{.x} in batches), and is hence particularly suited to lengthy
#'     computations or those which have variable or unpredicatable compute times
#'     over the indices, where it can be optimal to rely on \pkg{mirai}
#'     scheduling.
#'
#'     Note: daemons should generally be set prior to calling this function,
#'     otherwise ephemeral daemons will be created for each computation.
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

#' mirai Collect
#'
#' \code{mcollect} waits for and collects the data from \code{mwalk} or a list
#'     of \sQuote{mirai} objects.
#'
#' @param x a list of \sQuote{mirai} objects.
#'
#' @return For \code{mcollect}: a list, the same length as \code{x}, preserving
#'     names.
#'
#' @rdname mmap
#' @export
#'
mcollect <- function(x) lapply(lapply(x, call_mirai_), .subset2, "value")
