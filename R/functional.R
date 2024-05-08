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
#' @inheritParams mirai
#'
#' @return A list (the same length as \code{.x}, preserving names).
#'
#' @details This function is blocking and will wait for all results to be
#'     obtained. Daemons must also have been previously set with a call to
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
mmap <- function(.x, .f, ..., .args = list(), .compute = "default") {

  is.null(..[[.compute]]) && stop(._[["requires_daemons"]])
  vec <- vector(mode = "list", length = length(.x))
  nm <- names(.x)
  dots <- list(...)
  length(dots) && !is.null(names(dots)) && all(nzchar(names(dots))) ||
    stop(._[["named_args"]])

  for (i in seq_along(.x)) {
    vec[[i]] <- mirai(
      .expr = do.call(.f, c(list(.x), .args), quote = TRUE),
      .args = list(.f = .f, .x = .subset2(.x, i), .args = c(dots, .args)),
      .compute = .compute
    )
  }

  `names<-`(lapply(lapply(vec, call_mirai_), .subset2, "value"), nm)

}
