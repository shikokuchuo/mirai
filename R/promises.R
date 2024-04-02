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

# Aspects of this method, namely the re-throw and catch of the mirai error, is
# taken from code with the following licence:
#
# Copyright (c) 2016-2023 Posit Software, PBC
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

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
#' @exportS3Method promises::as.promise
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
          if (is_error_value(value) && !is_mirai_interrupt(value))
            tryCatch(stop(value), error = reject) else
              resolve(value)
        }
      query()
    }
  )
}
