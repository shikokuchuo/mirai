# Copyright (C) 2023-2025 Hibiki AI Limited <info@hibiki-ai.com>
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

#' Make mirai Promise
#'
#' Creates a \sQuote{promise} from a \sQuote{mirai}.
#'
#' This function is an S3 method for the generic `as.promise()` for class
#' \sQuote{mirai}.
#'
#' Requires the \CRANpkg{promises} package.
#'
#' Allows a \sQuote{mirai} to be used with the promise pipe `%...>%`, which
#' schedules a function to run upon resolution of the \sQuote{mirai}.
#'
#' @param x an object of class \sQuote{mirai}.
#'
#' @return A \sQuote{promise} object.
#'
#' @examplesIf interactive() && requireNamespace("promises", quietly = TRUE)
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
#' @exportS3Method promises::as.promise
#'
as.promise.mirai <- function(x) {

  promise <- .subset2(x, "promise")

  if (is.null(promise)) {

    promise <- if (unresolved(x)) {
      promises::promise(
        function(resolve, reject) .keep(x, environment())
      )$then(
        onFulfilled = function(value, .visible)
          if (is_error_value(value) && !is_mirai_interrupt(value))
            stop(if (is_mirai_error(value)) value else nng_error(value)) else
              value
      )
    } else {
      value <- .subset2(x, "value")
      promises::promise(
        function(resolve, reject)
          if (is_error_value(value) && !is_mirai_interrupt(value))
            reject(value) else
              resolve(value)
      )
    }

    base::`[[<-`(x, "promise", promise)

  }

  promise

}

#' Make mirai_map Promise
#'
#' Creates a \sQuote{promise} from a \sQuote{mirai_map}.
#'
#' This function is an S3 method for the generic `as.promise()` for class
#' \sQuote{mirai_map}.
#'
#' Requires the \CRANpkg{promises} package.
#'
#' Allows a \sQuote{mirai_map} to be used with the promise pipe `%...>%`, which
#' schedules a function to run upon resolution of the entire \sQuote{mirai_map}.
#'
#' Uses `promises::promise_all()`. If all of the promises were successful, the
#' returned promise will resolve to a list of the promise values; if any promise
#' fails, the first error to be encountered will be used to reject the returned
#' promise.
#'
#' @param x an object of class \sQuote{mirai_map}.
#'
#' @return A \sQuote{promise} object.
#'
#' @examplesIf interactive() && requireNamespace("promises", quietly = TRUE)
#' library(promises)
#'
#' with(daemons(1), {
#'   mp <- mirai_map(1:3, function(x) { Sys.sleep(1); x })
#'   p <- as.promise(mp)
#'   print(p)
#'   p %...>% print
#'   mp[.flat]
#' })
#'
#' @exportS3Method promises::as.promise
#'
as.promise.mirai_map <- function(x) {

  promise <- attr(x, "promise")

  if (is.null(promise)) {
    promise <- promises::promise_all(.list = lapply(x, promises::as.promise))
    attr(x, "promise") <- promise
  }

  promise

}

#' @exportS3Method promises::is.promising
#'
is.promising.mirai <- function(x) TRUE

#' @exportS3Method promises::is.promising
#'
is.promising.mirai_map <- function(x) TRUE
