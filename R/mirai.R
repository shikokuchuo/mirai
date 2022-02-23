# mirai ------------------------------------------------------------------------

#' mirai Server (Async Execution Daemon)
#'
#' Implements an executor/server for the remote process. Awaits data, evaluates
#'     an expression in an environment containing the supplied data, and returns
#'     the result to the caller/client.
#'
#' @inheritParams eval_mirai
#'
#' @return Invisible NULL.
#'
#' @keywords internal
#' @export
#'
exec <- function(url) {

  sock <- socket(protocol = "rep", dial = url)
  ctx <- context(sock)
  on.exit(expr = {
    send_aio(ctx, data = as.raw(0L), mode = "serial")
    close(sock)
  })
  envir <- recv_ctx(ctx, mode = "serial", keep.raw = FALSE)
  msg <- eval(expr = .subset2(envir, ".expr"), envir = envir)
  on.exit()
  send_ctx(ctx, data = msg, mode = "serial", echo = FALSE)
  Sys.sleep(2L)
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
#' mirai <- eval_mirai({
#'   res <- rnorm(n)
#'   res / rev(res)
#' }, n = 1e6)
#' call_mirai(mirai)
#' mirai$value
#' }
#'
#' @export
#'
eval_mirai <- function(.expr, ...) {

  mc <- match.call(expand.dots = FALSE)
  arglist <- list(.expr = .subset2(mc, ".expr"), ...)
  envir <- list2env(arglist)
  url <- sprintf("ipc:///tmp/n%.15f", runif(1L))
  cmd <- switch(.subset2(.Platform, "OS.type"),
                unix = file.path(R.home("bin"), "Rscript"),
                windows = file.path(R.home("bin"), "Rscript.exe"))
  func <- sprintf("mirai::exec(%s)", deparse(url))
  system2(command = cmd, args = c("--vanilla", "-e", shQuote(func)),
          stdout = NULL, stderr = NULL, wait = FALSE)
  sock <- socket(protocol = "req", listen = url)
  ctx <- context(sock)
  aio <- request(ctx, data = envir, send_mode = "serial", recv_mode = "serial", keep.raw = FALSE)
  mirai <- `class<-`(new.env(), "mirai")
  mirai[["aio"]] <- aio
  mirai[["socket"]] <- sock
  mirai

}

#' Call mirai (Retrieve Value)
#'
#' Retrieve the value of a mirai (optionally waiting for the the asynchronous
#'     operation to resolve if it is still in progress).
#'
#' @param mirai a 'mirai' object.
#' @param wait [default TRUE] whether to wait for completion of the asynschronous
#'     operation (blocking) or else return immediately. [experimental]
#'
#' @return The passed mirai (invisibly). The retrieved value is stored in
#'     \code{$value}. If the mirai has yet to resolve, NULL will be returned
#'     instead.
#'
#' @details This function will by default wait for the async operation to
#'     complete if it is still in progress. Specify the 'wait' argument to
#'     modify this behaviour.
#'
#'     If an error occured in evaluation, a nul byte \code{00} (or serialized
#'     nul byte) will be returned. \code{\link{is_nul_byte}} can be used to test
#'     for a nul byte.
#'
#'     The mirai updates itself in place, so do not assign the output of this
#'     function to avoid duplicates. To access the value of a mirai \code{x}
#'     directly, use \code{call_mirai(x)$value}.
#'
#' @section Non-waiting call:
#'
#'     To query whether mirai \code{x} has resolved, test if
#'     \code{call_mirai(x, wait = FALSE)} returns NULL. When the mirai resolves,
#'     the mirai itself will be returned (invisibly) instead of NULL. The mirai's
#'     return value may then be extracted using \code{x$value}.
#'
#'     It is inadvisable to try to extract the value of a mirai in one step using
#'     a non-waiting call, unless it is impossible for your expression to return
#'     NULL. This is as NULL$value is also NULL, hence it would otherwise not be
#'     possible to distinguish between an unresolved mirai and a NULL return value.
#'
#'     This feature has the tag [experimental], which indicates that it remains
#'     under development. Please note that the final implementation may differ
#'     from the current version.
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
#' mirai <- eval_mirai({
#'   res <- rnorm(n)
#'   res / rev(res)
#' }, n = 1e6)
#' call_mirai(mirai)
#' mirai$value
#' }
#'
#' @export
#'
call_mirai <- function(mirai, wait = TRUE) {

  if (length(.subset2(mirai, "aio"))) {
    if (!missing(wait) && !isTRUE(wait)) {
      is.null(call_aio(.subset2(mirai, "aio"), block = FALSE)) && return()
    } else {
      call_aio(.subset2(mirai, "aio"))
    }
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

