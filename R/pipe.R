# Copyright (C) 2022 Hibiki AI Limited <info@hibiki-ai.com>
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

# mirai - Deferred Evaluation Pipe ------------------------------------------

#' Deferred Evaluation Pipe
#'
#' Pipe a possibly unresolved value forward into a function.
#'
#' @param x a value that is possibly an 'unresolvedValue'.
#' @param f a function that accepts 'x' as its first argument.
#'
#' @return The evaluated result, or if x is an 'unresolvedValue', an
#'     'unresolvedExpr'.
#'
#' @details An 'unresolvedExpr' encapsulates the eventual evaluation result.
#'     Query its \code{$data} element for resolution. Once resolved, the object
#'     changes into a 'resolvedExpr' and the evaluated result will be available
#'     at \code{$data}.
#'
#'     Supports stringing together a series of piped expressions (as per
#'     the below example).
#'
#'     \code{\link{unresolved}} may be used on an 'unresolvedExpr' or its
#'     \code{$data} element to test for resolution.
#'
#' @section Usage:
#'
#'     Usage is similar to R's native \code{|>} pipe.
#'
#'     \code{x \%>>\% f} is equivalent to \code{f(x)}
#'
#'     \code{x \%>>\% f()} is equivalent to \code{f(x)}
#'
#'     \code{x \%>>\% f(y)} is equivalent to \code{f(x, y)}
#'
#'     Please note that other usage is not supported and it is not a drop-in
#'     replacement for magrittr's \code{\%>\%} pipe.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- mirai({Sys.sleep(0.5); 1})
#' b <- m$data %>>% c(2, 3) %>>% as.character()
#' b
#' b$data
#' call_mirai(m)
#' b$data
#' b
#'
#' }
#'
#' @export
#'
`%>>%` <- function(x, f) {
  if (unresolved(x)) {
    mc <- match.call()
    data <- NULL
    env <- `class<-`(new.env(), c("unresolvedExpr", "unresolvedValue"))
    makeActiveBinding(sym = "data", fun = function(x) {
      if (is.null(data)) {
        data <- eval(mc, envir = parent.frame(), enclos = NULL)
        if (!inherits(data, "unresolvedExpr")) `class<-`(env, "resolvedExpr")
      }
      data
    }, env = env)
    env
  } else {
    x <- substitute(x)
    y <- substitute(f)
    if (is.symbol(y)) {
      eval(as.call(c(y, x)), envir = parent.frame(2L), enclos = NULL)
    } else {
      f <- y[[1L]]
      y[[1L]] <- NULL
      eval(as.call(c(f, x, y)), envir = parent.frame(2L), enclos = NULL)
    }
  }
}

#' @export
#'
print.unresolvedExpr <- function(x, ...) {

  cat("< unresolvedExpr >\n - $data to query resolution\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.resolvedExpr <- function(x, ...) {

  cat("< resolvedExpr: $data >\n", file = stdout())
  invisible(x)

}

