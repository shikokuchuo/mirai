# mirai ------------------------------------------------------------------------

#' mirai Server (Async Execution Daemon)
#'
#' Implements an executor/server to be run in the remote process. Awaits data,
#'     evaluates an expression in an environment containing the supplied data,
#'     and returns the result to the caller/client.
#'
#' @inheritParams eval_mirai
#'
#' @return Invisible NULL.
#'
#' @keywords internal
#' @export
#'
exec <- function(expr, url) {

  sock <- socket(protocol = "rep", listen = url)
  ctx <- context(sock)
  on.exit(expr = send_aio(ctx, data = as.raw(0L), mode = "serial"))
  envir <- recv_ctx(ctx, mode = "serial", keep.raw = FALSE)
  msg <- eval(expr = substitute(expr), envir = envir)
  on.exit(expr = NULL)
  send_ctx(ctx, data = msg, mode = "serial", echo = FALSE)
  Sys.sleep(1L)
  close(sock)

}

#' Eval mirai (Evaluate Async)
#'
#' Evaluate an expression asynchronously in a new non-blocking R process. This
#'     function will return immediately with a mirai, which can be called for
#'     the result.
#'
#' @param .expr an expression to evaluate in a new R process.
#' @param ... named arguments specifying the variables contained in 'expr'.
#'
#' @return A 'mirai' object.
#'
#' @details This function will return immediately. To call the result, use
#'     \code{\link{call_mirai}} on the returned 'mirai' object.
#'
#'     The expression '.expr' will be evaluated in a new R process in a clean
#'     environment consisting of the named objects passed as '...' only.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' mirai <- eval_mirai(x + y + 1, x = 2, y = 3)
#' mirai
#' call_mirai(mirai)
#' mirai
#' mirai$value
#'
#' mirai <- eval_mirai(as.matrix(df), df = data.frame())
#' call_mirai(mirai)$value
#'
#' mirai <- eval_mirai(rnorm(n), n = 1e6)
#' call_mirai(mirai)
#' str(mirai$value)
#' }
#'
#' @export
#'
eval_mirai <- function(.expr, ...) {

  dots <- list(...)
  envir <- list2env(dots)
  url <- paste0("ipc:///tmp/n", runif(1L))
  cmd <- switch(.subset2(.Platform, "OS.type"),
                unix = paste0(R.home("bin"), "/Rscript"),
                windows = paste0(R.home("bin"), "/Rscript.exe"))
  func <- paste0("mirai::exec(", deparse(substitute(.expr)), ",", deparse(url), ")")
  system2(command = cmd, args = c("-e", shQuote(func)),
          stdout = NULL, stderr = NULL, wait = FALSE)
  sock <- socket(protocol = "req", dial = url)
  ctx <- context(sock)
  aio <- request(ctx, data = envir, send_mode = "serial", recv_mode = "serial", keep.raw = FALSE)
  mirai <- `class<-`(new.env(), "mirai")
  mirai[["aio"]] <- aio
  mirai[["socket"]] <- sock
  mirai

}

#' Call mirai (Retrieve Value)
#'
#' Retrieve the value of a mirai (waiting for the the asynchronous operation to
#'     resolve if it is still in progress).
#'
#' @param mirai a 'mirai' object.
#'
#' @return The passed mirai (invisibly). The retrieved value is stored in
#'     \code{$value}.
#'
#' @details This function will wait for the async operation to complete if it is
#'     still in progress.
#'
#'     If an error occured in evaluation, a nul byte \code{00} (or serialized
#'     nul byte) will be returned.
#'
#'     The mirai updates itself in place, so do not assign the output of this
#'     function to avoid duplicates. To access the value of a mirai \code{x}
#'     directly, use \code{call_mirai(x)$value}.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' mirai <- eval_mirai(x + y + 1, x = 2, y = 3)
#' mirai
#' call_mirai(mirai)
#' mirai
#' mirai$value
#'
#' mirai <- eval_mirai(as.matrix(df), df = data.frame())
#' call_mirai(mirai)$value
#'
#' mirai <- eval_mirai(rnorm(n), n = 1e6)
#' call_mirai(mirai)
#' str(mirai$value)
#' }
#'
#' @export
#'
call_mirai <- function(mirai) {

  if (length(.subset2(mirai, "aio"))) {
    call_aio(.subset2(mirai, "aio"))
    close(.subset2(mirai, "socket"))
    rm("socket", envir = mirai)
    mirai[["value"]] <- .subset2(.subset2(mirai, "aio"), "data")
    rm("aio", envir = mirai)
  }
  invisible(mirai)

}

#' @export
#'
print.mirai <- function(x, ...) {

  cat("< mirai >\n")
  if (length(.subset2(x, "aio"))) {
    cat(" ~ use call_mirai() to resolve\n")
  } else {
    cat(" - $value for evaluated result\n")
  }
  invisible(x)

}

