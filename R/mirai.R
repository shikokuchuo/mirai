# mirai ------------------------------------------------------------------------

#' mirai Server (Async Executor)
#'
#' Implements an executor/server for the remote process. Awaits data, evaluates
#'     an expression in an environment containing the supplied data, and returns
#'     the result to the caller/client.
#'
#' @param . the internally assigned unique URL.
#'
#' @return Integer exit code.
#'
#' @keywords internal
#' @export
#'
. <- function(.) {

  missing(.) && stop("this function is only to be called internally by the package")
  sock <- socket(protocol = "rep", dial = .)
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
#' @param ... named arguments specifying the variables contained in '.expr'.
#'
#' @return A 'mirai' object.
#'
#' @details This function will return a 'mirai' object immediately. The value of
#'     a mirai may be accessed at any time at \code{$data}, and if yet to
#'     resolve, a value of logical NA 'unresolved value' will be returned instead.
#'
#'     To call (and wait for) the result, use \code{\link{call_mirai}} on the
#'     returned 'mirai' object.
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
#' mirai$data
#'
#' mirai <- eval_mirai(as.matrix(df), df = data.frame())
#' call_mirai(mirai)$data
#'
#' mirai <- eval_mirai({
#'   res <- rnorm(n)
#'   res / rev(res)
#' }, n = 1e6)
#' call_mirai(mirai)
#' mirai$data
#' }
#'
#' @export
#'
eval_mirai <- function(.expr, ...) {

  if (!is.null(avail <- attr(mirai(), "daemons")) && avail) {

    arglist <- list(.expr = substitute(.expr), ...)
    envir <- list2env(arglist)
    ctx <- context(mirai())
    aio <- request(ctx, data = envir, send_mode = "serial", recv_mode = "serial", keep.raw = FALSE)
    `[[<-`(aio, "con", ctx)
    `class<-`(aio, c("mirai", class(aio)))

  } else {

    arglist <- list(.expr = substitute(.expr), ...)
    envir <- list2env(arglist)
    platform <- .subset2(.Platform, "OS.type")
    cmd <- switch(platform,
                  unix = file.path(R.home("bin"), "Rscript"),
                  windows = file.path(R.home("bin"), "Rscript.exe"))
    url <- sprintf("ipc:///tmp/n%.15f", runif(1L))
    arg <- c("--vanilla", "-e", shQuote(sprintf("mirai::.(%s)", deparse(url))))
    system2(command = cmd, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
    sock <- socket(protocol = "req", listen = url)
    ctx <- context(sock)
    aio <- request(ctx, data = envir, send_mode = "serial", recv_mode = "serial", keep.raw = FALSE)
    attr(sock, "context") <- ctx
    `[[<-`(aio, "con", sock)
    `class<-`(aio, c("mirai", class(aio)))

  }

}

#' Call mirai (Retrieve Value)
#'
#' Retrieve the value of a mirai (waiting for the the asynchronous operation to
#'     resolve if it is still in progress).
#'
#' @param mirai a 'mirai' object.
#'
#' @return The passed mirai (invisibly). The retrieved value is stored in
#'     \code{$data}.
#'
#' @details This function will wait for the async operation to complete if still
#'     in progress (blocking).
#'
#'     Alternatively, the value of a mirai may be accessed
#'     at any time at \code{$data}, and if yet to resolve, a value of logical NA
#'     'unresolved value' will be returned instead.
#'
#'     If an error occured in evaluation, a nul byte \code{00} (or serialized
#'     nul byte) will be returned. \code{\link{is_nul_byte}} can be used to test
#'     for a nul byte.
#'
#'     The mirai updates itself in place, so do not assign the output of this
#'     function to avoid duplicates. To access the value of a mirai \code{x}
#'     directly, use \code{call_mirai(x)$data}.
#'
#'     To query completion of a mirai, \code{\link{unresolved}} may also be used.
#'     This returns TRUE only if the mirai is yet to resolve, and is suitable
#'     for use in control flow statements such as \code{while} or \code{if}.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' mirai <- eval_mirai(x + y + 1, x = 2, y = 3)
#' mirai
#' mirai$data
#'
#' mirai <- eval_mirai(as.matrix(df), df = data.frame())
#' call_mirai(mirai)$data
#'
#' mirai <- eval_mirai({
#'   res <- rnorm(n)
#'   res / rev(res)
#' }, n = 1e6)
#' call_mirai(mirai)
#' mirai$data
#' }
#'
#' @export
#'
call_mirai <- function(mirai) {

  if (!is.null(.subset2(mirai, "con"))) {
    call_aio(mirai)
    close(.subset2(mirai, "con"))
    rm("con", envir = mirai)
  }

  invisible(mirai)

}

#' mirai Server (Async Execution Daemon)
#'
#' Implements a persistent executor/server for the remote process. Awaits data,
#'     evaluates an expression in an environment containing the supplied data,
#'     and returns the result to the caller/client.
#'
#' @inheritParams .
#'
#' @return Integer exit code.
#'
#' @keywords internal
#' @export
#'
.. <- function(.) {

  missing(.) && stop("this function is only to be called internally by the package")
  sock <- socket(protocol = "rep", dial = .)
  on.exit(expr = {
    send_aio(ctx, data = as.raw(0L), mode = "serial")
    close(sock)
    ..(.)
  })
  while (TRUE) {
    ctx <- context(sock)
    envir <- recv_ctx(ctx, mode = "serial", keep.raw = FALSE)
    missing(envir) && break
    msg <- eval(expr = .subset2(envir, ".expr"), envir = envir)
    send_ctx(ctx, data = msg, mode = "serial", echo = FALSE)
    close(ctx)
  }

  on.exit()
  send_aio(ctx, data = as.raw(1L), mode = "serial")
  close(sock)

}

#' mirai (Daemon Manager)
#'
#' Set the number of daemons (background processes). Use this function
#'     to create persistent background processes to send \code{\link{eval_mirai}}
#'     requests. Setting a positive number of daemons provides a potentially
#'     more efficient solution for async operations as new processes do not need
#'     to be spun up on an ad hoc basis. [Experimental]
#'
#' @param set_daemons integer number of background processes.
#'
#' @return The return value will depend on whether background processes are
#'     created or destroyed (see details section). Without specifying any
#'     arguments, the 'nanoSocket' for connecting to the background
#'     processes, or NULL if it has yet to be created.
#'
#' @details Background processes will be created or destroyed as appropriate.
#'     \itemize{
#'     \item{If new processes are created, the return value will be the integer
#'     number of created processes.}
#'     \item{If processes are destroyed, the return value will be a list of the
#'     exit signals from each destroyed process (an integer byte 1 on success).
#'     This process will wait for confirmation to be received.}
#'     \item{Otherwise NULL will be returned invisibly.}
#'     }
#'
#'     mirai will revert to the default behaviour of creating a new background
#'     process for each request if the number of daemons is set to 0.
#'
#'     This feature has the tag [experimental], which indicates that it remains
#'     under development. Please note that the final implementation may differ
#'     from the current version.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#' # To spin up 4 background processes
#' mirai(4)
#' # To destroy them all
#' mirai(0)
#' }
#'
#' @export
#'
mirai <- function(...) {

  daemons <- 0L
  url <- sock <- cmd <- arg <- NULL

  function(set_daemons) {
    if (missing(set_daemons)) {

      sock

    } else {

      set_daemons <- as.integer(set_daemons)
      set_daemons >= 0L || stop("number of daemons must be zero or greater")
      if (is.null(url)) {
        url <<- sprintf("ipc:///tmp/n%.15f", runif(1L))
        sock <<- socket(protocol = "req", listen = url)
        cmd <<- switch(.subset2(.Platform, "OS.type"),
                       unix = file.path(R.home("bin"), "Rscript"),
                       windows = file.path(R.home("bin"), "Rscript.exe"))
        arg <<- c("--vanilla", "-e", shQuote(sprintf("mirai::..(%s)", deparse(url))))
      }

      delta <- set_daemons - daemons

      if (delta > 0L) {
        original <- daemons
        for (i in seq_len(delta)) {
          system2(command = cmd, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
          daemons <<- daemons + 1L
        }
        attr(sock, "daemons") <- daemons
        daemons - original

      } else if (delta < 0L) {
        res <- vector(mode = "list", length = -delta)
        for (i in seq_len(-delta)) {
          ctx <- context(sock)
          aio <- request(ctx, data = .mirai_scm(), send_mode = "serial", recv_mode = "serial", keep.raw = FALSE)
          call_aio(aio)
          close(ctx)
          if (!identical(.subset2(aio, "data"), as.raw(1L))) message(Sys.time(), " [ shutdown failure ] process: ", i)
          res[[i]] <- .subset2(aio, "data")
          daemons <<- daemons - 1L
        }
        attr(sock, "daemons") <- daemons
        res

      } else {
        invisible()
      }

    }

  }

}

#' @export
#'
print.mirai <- function(x, ...) {

  cat("< mirai >\n - $data for evaluated result\n", file = stdout())
  invisible(x)

}

