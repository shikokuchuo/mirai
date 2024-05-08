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

#' mirai lapply
#'
#' Apply a function over a list or vector using \pkg{mirai}.
#'
#' @param X a vector (atomic or list) or an expression object.
#' @param FUN the function to be applied to each element of X.
#' @param ... optional arguments to \code{FUN}.
#' @inheritParams mirai
#'
#' @return A list (the same length as X, preserving names).
#'
#' @details This function is blocking and will wait for all results to be
#'     obtained.
#'
#' @examples
#' with(
#'   daemons(1, dispatcher = FALSE),
#'   mlapply(1:3, rnorm, mean = 20)
#' )
#'
#' @export
#'
mlapply <- function(X, FUN, ..., .compute = "default") {

  x <- vector(mode = "list", length = length(X))
  fun <- names(X)

  for (i in seq_along(X)) {
    x[[i]] <- mirai(
      .expr = do.call(fun, c(list(x), args), quote = TRUE),
      .args = list(fun = FUN, x = .subset2(X, i), args = list(...))
    )
  }

  `names<-`(lapply(lapply(x, call_mirai_), .subset2, "value"), fun)

}
