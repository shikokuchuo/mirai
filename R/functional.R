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

# mirai functional -------------------------------------------------------------

#' mirai Walk
#'
#' Walk a function over a list or vector using \pkg{mirai} for the side-effects.
#'
#' @param .x a list or atomic vector.
#' @param .f a function to be applied to each element of \code{.x}.
#' @param ... optional arguments to \code{.f}.
#' @param .args optional arguments to \code{.f} provided as a list.
#' @inheritParams mirai
#'
#' @return An unnamed list of \sQuote{mirai}, the same length as \code{.x}.
#'
#' @details This function sends each application of \code{.f} on an element of
#'     \code{.x} for computation in a separate \code{\link{mirai}} call. It does
#'     not wait for completion.
#'
#'     Whilst this function is designed primarily to enact side effects, it may
#'     also be used as an asynchronous map function as \sQuote{mirai} are
#'     returned, and their values may be collected.
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
#' ml <- mwalk(1:3, rnorm, mean = 20, .args = list(sd = 2))
#' ml
#'
#' }
#'
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
  vec

}

#' mirai Map
#'
#' Map a function over a list or vector using \pkg{mirai}.
#'
#' @inheritParams mwalk
#' @param .progress [default FALSE] if TRUE, reports progress via a simple text
#'     progress indicator to \sQuote{stderr}.
#' @param .stop [default FALSE] errors are returned as \sQuote{miraiError} /
#'     \sQuote{errorValue} as the case may be, allowing recovery from partial
#'     failure. If TRUE, performs early stopping as soon as an error is
#'     encountered, with remaining computations aborted.
#'
#' @return A list, the same length as \code{.x}, preserving names.
#'
#' @details This function sends each application of \code{.f} on an element of
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
#' mmap(list(1, "a", 3), sum)
#'
#' }
#'
#' @export
#'
mmap <- function(.x, .f, ..., .args = list(), .progress = FALSE, .stop = FALSE, .compute = "default") {

  vec <- mwalk(.x = .x, .f = .f, ..., .args = .args, .compute = .compute)

  if (.progress || .stop) {
    xlen <- length(.x)
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

  } else {
    lapply(vec, call_mirai_)
  }

  `names<-`(lapply(vec, .subset2, "value"), names(.x))

}
