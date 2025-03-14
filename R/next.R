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

# mirai ------------------------------------------------------------------------

#' Next >> Developer Interface
#'
#' `nextstream` retrieves the currently stored L'Ecuyer-CMRG RNG stream
#' for the specified compute profile and advances it to the next stream.
#'
#' These functions are exported for use by packages extending \pkg{mirai} with
#' alternative launchers of [daemon()] processes.
#'
#' For `nextstream`: This function should be called for its return value
#' when required. The function also has the side effect of automatically
#' advancing the stream stored within the compute profile. This ensures that the
#' next recursive stream is returned when the function is called again.
#'
#' @inheritParams mirai
#'
#' @return For `nextstream`: a length 7 integer vector, as given by
#'   `.Random.seed` when the L'Ecuyer-CMRG RNG is in use (may be passed directly
#'   to the `rs` argument of [daemon()]), or else NULL if a stream has not yet
#'   been created.
#'
#' @examplesIf interactive()
#' daemons(1L)
#' nextstream()
#' nextstream()
#'
#' nextget("pid")
#' nextget("urls")
#'
#' daemons(0)
#'
#' @keywords internal
#' @export
#'
nextstream <- function(.compute = "default") next_stream(..[[.compute]])

#' Next >> Developer Interface
#'
#' `nextget` retrieves the specified item from the specified compute profile.
#'
#' @param x character value of item to retrieve. One of `"n"` (number of
#'   dispatcher daemons), `"pid"` (dispatcher process ID), `"urls"` (URLs
#'   dispatcher is listening at) or `"tls"` (the stored client TLS configuration
#'   for use by daemons).
#'
#' @return For `nextget`: the requested item, or else NULL if not present.
#'
#' @keywords internal
#' @rdname nextstream
#' @export
#'
nextget <- function(x, .compute = "default") ..[[.compute]][[x]]

#' Next >> Developer Interface
#'
#' `nextcode` translates integer exit codes returned by [daemon()].
#'
#' @param xc integer return value of [daemon()].
#'
#' @return For `nextcode`: character string.
#'
#' @examples
#' nextcode(0L)
#' nextcode(1L)
#'
#' @keywords internal
#' @rdname nextstream
#' @export
#'
nextcode <- function(xc)
  sprintf("%d | Daemon %s", xc, switch(xc + 1L, "connection terminated", "idletime limit reached", "walltime limit reached", "task limit reached"))

# internals --------------------------------------------------------------------

next_stream <- function(envir) {
  stream <- envir[["stream"]]
  if (length(stream)) `[[<-`(envir, "stream", parallel::nextRNGStream(stream))
  stream
}

next_msgid <- function(envir) {
  msgid <- envir[["msgid"]] + 1L
  `[[<-`(envir, "msgid", msgid)
  msgid
}
