# Copyright (C) 2023 Hibiki AI Limited <info@hibiki-ai.com>
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

# mirai ------------------------------------------------------------------------

#' Next >> Developer Interface
#'
#' \code{nextstream} retrieves the currently stored L'Ecuyer-CMRG RNG stream
#'     for the specified compute profile and advances it to the next stream.
#'
#' @inheritParams mirai
#'
#' @return For \code{nextstream}: a length 7 integer vector, as given by \code{.Random.seed}
#'     when the L'Ecuyer-CMRG RNG is in use (may be passed directly to the 'rs'
#'     argument of \code{\link{daemon}}), or else NULL if a stream has not yet
#'     been created.
#'
#' @details These functions are exported for use by packages extending
#'     \CRANpkg{mirai} with alternative launchers of \code{\link{daemon}}
#'     processes.
#'
#'     For \code{nextstream}: This function should be called for its return value
#'     when required. The function also has the side effect of automatically
#'     advancing the stream stored within the compute profile. This ensures that
#'     next time the function is called the correct value will be returned.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' daemons(1L)
#' nextstream()
#' nextstream()
#'
#' nextget("pid")
#' nextget("urls")
#'
#' daemons(0)
#'
#' }
#'
#' @export
#'
nextstream <- function(.compute = "default") next_stream(..[[.compute]])

#' Next >> Developer Interface
#'
#' \code{nextget} retrieves the specified item from the specified compute
#'     profile.
#'
#' @param x character value of item to retrieve. One of 'pid' (dispatcher process
#'     ID), 'urls' (URLs dispatcher is listening at) or 'tls' (the stored client
#'     TLS configuration to be sent to daemons).
#'
#' @return For \code{nextget}: the requested item, or else NULL if not present.
#'
#' @rdname nextstream
#' @export
#'
nextget <- function(x, .compute = "default") ..[[.compute]][[x]]

# internals --------------------------------------------------------------------

next_stream <- function(envir) {
  stream <- envir[["stream"]]
  length(stream) || return()
  `[[<-`(envir, "stream", nextRNGStream(stream))
  stream
}
