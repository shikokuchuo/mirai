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

# Joe Cheng (3 Apr 2024):
#
# We are going through some effort here to ensure that any error we raise here
# has "deep stacks" preserved while run in a Shiny app. For this to happen, we
# either need to raise the error while the `as.promise.mirai` call is still on
# the call stack, or, we are within an `onFulfilled` or `onRejected` callback
# from a `promises::then` call (assuming that `then()` was called while
# `as.promise.mirai` was still on the call stack).
#
# The only way we would violate those rules is by raising the error from
# within a `later::later` callback. So this code is factored to isolate that
# `later::later` code

#' Make Mirai Promise
#'
#' Creates a \sQuote{promise} from a \sQuote{mirai}.
#'
#' @param x an object of class \sQuote{mirai}.
#'
#' @return A \sQuote{promise} object.
#'
#' @details This function is an S3 method for the generic \code{as.promise} for
#'     class \sQuote{mirai}.
#'
#'     Requires the \CRANpkg{promises} package.
#'
#'     Allows a \sQuote{mirai} to be used with the promise pipe \code{\%...>\%},
#'     which schedules a function to run upon resolution of the \sQuote{mirai}.
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
#'
as.promise.mirai <- function(x) {

  promise <- .subset2(x, "promise")

  if (is.null(promise)) {

    promise <- if (.unresolved(x)) {
      promises::promise(
        function(resolve, reject) .keep(x, environment())
      )$then(
        onFulfilled = function(value, .visible)
          if (is_error_value(value) && !is_mirai_interrupt(value))
            stop(if (is_mirai_error(value)) value else nng_error(value)) else
              value
      )
    } else {
      value <- collect_aio(x)
      promises::promise(
        function(resolve, reject)
          if (is_error_value(value) && !is_mirai_interrupt(value))
            reject(value) else
              resolve(value)
      )
    }

    assign("promise", promise, x)

  }

  promise

}

#' @exportS3Method promises::is.promising
#'
is.promising.mirai <- function(x) TRUE
