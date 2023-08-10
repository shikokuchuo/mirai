# Copyright (C) 2022-2023 Hibiki AI Limited <info@hibiki-ai.com>
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
#' Pipe a possibly unresolved value forward into a function. The piped expression
#'     should be wrapped in \code{.()}.
#'
#' @param x a 'mirai' or mirai value at \code{$data} that is possibly an
#'     'unresolvedValue'.
#' @param f a function that accepts 'x' as its first argument.
#' @param expr a piped expression.
#'
#' @return The evaluated result, or if the mirai value of x is an
#'     'unresolvedValue', an 'unresolvedExpr'.
#'
#'     It is advisable to wrap \code{resolve()} around a piped expression to
#'     ensure stability of return types, as this is guaranteed to return either
#'     an 'unresolvedExpr' or 'resolvedExpr'.
#'
#' @details An 'unresolvedExpr' encapsulates the eventual evaluation result.
#'     Query its \code{$data} element for resolution. Once resolved, the object
#'     changes into a 'resolvedExpr' and the evaluated result will be available
#'     at \code{$data}.
#'
#'     Supports stringing together a series of piped expressions (as per
#'     the below example).
#'
#'     Wrap a piped expression in \code{.()} to ensure that the return value is
#'     always an 'unresolvedExpr' or 'resolvedExpr' as the case may be,
#'     otherwise if 'x' is already resolved, the evaluated result would be
#'     returned directly.
#'
#'     \code{\link{unresolved}} may be used on an expression or its \code{$data}
#'     element to test for resolution.
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
#' b <- .(m %>>% c(2, 3) %>>% as.character)
#' unresolved(b)
#' b
#' b$data
#'
#' call_mirai(m)
#' unresolved(b)
#' b
#' b$data
#'
#' }
#'
#' @rdname deferred-execution-pipe
#' @export
#'
`%>>%` <- function(x, f) {
  if (unresolved(x)) {
    syscall <- sys.call()
    data <- NULL
    env <- `class<-`(new.env(hash = FALSE, parent = parent.frame()), c("unresolvedExpr", "unresolvedValue"))
    makeActiveBinding(sym = "data", fun = function(x) {
      if (is.null(data)) {
        data <- eval(syscall, envir = env, enclos = NULL)
        if (!inherits(data, "unresolvedExpr")) `class<-`(env, "resolvedExpr")
      }
      data
    }, env = env)
    env
  } else {
    x <- if (is_mirai(x)) `[[<-`(quote(.subset2(x, "data")), 2L, substitute(x)) else substitute(x)
    y <- substitute(f)
    if (is.symbol(y)) {
      eval.parent(as.call(c(y, x)))
    } else {
      f <- y[[1L]]
      y[[1L]] <- NULL
      eval.parent(as.call(c(f, x, y)))
    }
  }
}

#' @rdname deferred-execution-pipe
#' @export
#'
. <- function(expr)
  if (inherits(expr, c("unresolvedExpr", "resolvedExpr"))) expr else
    `class<-`(`[[<-`(new.env(hash = FALSE), "data", expr), "resolvedExpr")

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
