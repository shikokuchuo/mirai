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
#' @return A list (the same length as \code{.x}, preserving names).
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
#'     Daemons should have previously been set with a call to
#'     \code{\link{daemons}}; if not then operations will be performed in a
#'     single ephemeral daemon and a warning is output.
#'
#' @examples
#' with(
#'   daemons(1, dispatcher = FALSE),
#'   mmap(1:3, rnorm, mean = 20, .args = list(sd = 2))
#' )
#'
#' if (interactive()) {
#'
#' mmap(seq(from = 0.1, to = 0.4, by = 0.1), Sys.sleep, .progress = TRUE)
#'
#' }
#'
#' @export
#'
mmap <- function(.x, .f, ..., .args = list(), .progress = FALSE, .stop = FALSE, .compute = "default") {

  is.null(..[[.compute]]) && {
    warning(._[["requires_daemons"]], immediate. = TRUE)
    return(
      with(
        daemons(n = 1L, dispatcher = FALSE, resilience = FALSE, cleanup = FALSE, .compute = .compute),
        eval(sys.call())
      )
    )
  }
  xlen <- length(.x)
  vec <- vector(mode = "list", length = xlen)

  for (i in seq_len(xlen))
    vec[[i]] <- mirai(
      .expr = do.call(.f, c(list(.x), .args)),
      .args = list(.f = .f, .x = .subset2(.x, i), .args = c(list(...), .args)),
      .compute = .compute
    )

  if (.progress || .stop) {
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
