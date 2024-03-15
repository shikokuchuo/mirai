# Copyright (C) 2023-2024 Hibiki AI Limited <info@hibiki-ai.com>
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

# mirai.promises ---------------------------------------------------------------

#' Make Mirai Promise
#'
#' Creates a 'promise' from a 'mirai'.
#'
#' @param x an object of class 'mirai'.
#'
#' @return A 'promise' object.
#'
#' @details This function is an S3 method for the generic \code{as.promise} for
#'     class 'mirai'.
#'
#'     Requires the \CRANpkg{promises} package.
#'
#'     Allows a 'mirai' to be used with the promise pipe \code{\%...>\%}, which
#'     schedules a function to run upon resolution of the 'mirai'.
#'
#' @examples
#' if (interactive() && requireNamespace("promises", quietly = TRUE)) {
#'
#' library(promises)
#'
#' p <- as.promise(mirai("example"))
#' print(p)
#' is.promise(p)
#'
#' p2 <- mirai("completed") %...>% identity()
#' p2$then(cat)
#' is.promise(p2)
#'
#' }
#'
#' @method as.promise mirai
#' @export
#'
as.promise.mirai <- function(x) {
  promises::promise(
    function(resolve, reject) {
      query <- function()
        if (unresolved(x)) {
          later::later(query, delay = 0.1)
        } else {
          value <- .subset2(x, "value")
          if (mirai::is_error_value(value) && !mirai::is_mirai_interrupt(value))
            tryCatch(stop(value), error = reject) else
              resolve(value)
        }
      query()
    }
  )
}
