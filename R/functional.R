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

#' mirai Map
#'
#' Map a function over a list or vector using \pkg{mirai}.
#'
#' @param .x a list, atomic vector, or an expression object.
#' @param .f a function to be applied to each element of \code{.x}.
#' @param ... optional arguments to \code{.f}.
#' @param .args optional arguments to \code{.f} provided as a list.
#' @param .stop [default FALSE] all errors are returned as \sQuote{miraiError} /
#'     \sQuote{errorValue} as the case may be, allowing recovery from partial
#'     failure. If TRUE, performs early stopping (with the error message) as
#'     soon as an error is encountered (remaining computations are aborted).
#' @inheritParams mirai
#'
#' @return A list (the same length as \code{.x}, preserving names).
#'
#' @details This function sends each application of \code{.f} on an element of
#'     \code{.x} for computation in a separate \code{\link{mirai}} call, and
#'     waits for completion.
#'
#'     Designed to facilitate recovery from partial failure by returning all
#'     results by default, allowing only the failures to be re-run.
#'     Alternatively, there is the option for early stopping, which stops at the
#'     first failure and aborts all remaining computations.
#'
#'     Note: daemons must have been previously set with a call to
#'     \code{\link{daemons}}.
#'
#' @examples
#' with(
#'   daemons(1, dispatcher = FALSE),
#'   mmap(1:3, rnorm, mean = 20, .args = list(sd = 2))
#' )
#'
#' @export
#'
mmap <- function(.x, .f, ..., .args = list(), .stop = FALSE, .compute = "default") {

  is.null(..[[.compute]]) && stop(._[["requires_daemons"]])
  xlen <- length(.x)
  vec <- vector(mode = "list", length = xlen)

  for (i in seq_len(xlen))
    vec[[i]] <- mirai(
      .expr = do.call(.f, c(list(.x), .args), quote = TRUE),
      .args = list(.f = .f, .x = .subset2(.x, i), .args = c(list(...), .args)),
      .compute = .compute
    )

  for (i in seq_len(xlen)) {
    r <- .subset2(call_mirai_(vec[[i]]), "value")
    .stop && is_error_value(r) && {
      lapply(vec, stop_aio)
      stop(r)
    }
    vec[[i]] <- r
  }

  `names<-`(vec, names(.x))

}
