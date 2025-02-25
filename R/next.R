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
#' @keywords internal
#' @export
#'
nextstream <- function(.compute = "default") next_stream(..[[.compute]])

#' Next >> Developer Interface
#'
#' `nextget` retrieves the specified item from the specified compute profile.
#'
#' @param x character value of item to retrieve. One of \sQuote{n} (number of
#'   dispatcher daemons), \sQuote{pid} (dispatcher process ID), \sQuote{urls}
#'   (URLs dispatcher is listening at) or \sQuote{tls} (the stored client TLS
#'   configuration for use by daemons).
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
#' @param xc integer value.
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
nextcode <- function(xc) {
  str <- switch(xc + 1L,
    "0 | Daemon connection terminated",
    "1 | Daemon idletime limit reached",
    "2 | Daemon walltime limit reached",
    "3 | Daemon task limit reached"
  )
  is.null(str) && return("")
  str
}

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
