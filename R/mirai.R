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
#' @noRd
#'
. <- function(.) {

  sock <- socket(protocol = "rep", dial = ., autostart = TRUE)
  ctx <- context(sock)
  on.exit(expr = {
    send(ctx, data = as.raw(0L), mode = "serial", echo = FALSE)
    close(sock)
  })
  envir <- recv(ctx, mode = "serial", keep.raw = FALSE)
  msg <- eval(expr = .subset2(envir, ".expr"), envir = envir)
  on.exit()
  send(ctx, data = msg, mode = "serial", echo = FALSE)
  Sys.sleep(2L)
  close(sock)

}

#' mirai (Evaluate Async)
#'
#' Evaluate an expression asynchronously in a new background R process. This
#'     function will return immediately with a 'mirai', which will resolve to
#'     the evaluated result once complete.
#'
#' @param .expr an expression to evaluate in a new R process. This may be of
#'     arbitrary length, wrapped in \{\} if necessary.
#' @param ... named arguments specifying the variables contained in '.expr'.
#'
#' @return A 'mirai' object.
#'
#' @details This function will return a 'mirai' object immediately.
#'
#'     The value of a 'mirai' may be accessed at any time at \code{$data}, and
#'     if yet to resolve, an 'unresolved' logical NA will be returned instead.
#'
#'     \code{\link{unresolved}} may also be used on a 'mirai', which returns TRUE
#'     only if a 'mirai' has yet to resolve and FALSE otherwise. This is suitable
#'     for use in control flow statements such as \code{while} or \code{if}.
#'
#'     Alternatively, to call (and wait for) the result, use
#'     \code{\link{call_mirai}} on the returned 'mirai' object. This will block
#'     until the result is returned.
#'
#'     The expression '.expr' will be evaluated in a new R process in a clean
#'     environment consisting of the named objects passed as '...' only.
#'
#'     \code{\link{mirai}} is an alias for \code{\link{eval_mirai}}.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- mirai(x + y + 1, x = 2, y = 3)
#' m
#' m$data
#' Sys.sleep(0.2)
#' m$data
#'
#' m <- mirai(as.matrix(df), df = data.frame())
#' call_mirai(m)$data
#'
#' m <- mirai({
#'   res <- rnorm(n)
#'   res / rev(res)
#' }, n = 1e6)
#' while (unresolved(m)) {
#'   cat("unresolved\n")
#'   Sys.sleep(0.1)
#' }
#' m$data
#'
#' }
#'
#' @export
#'
eval_mirai <- function(.expr, ...) {

  missing(.expr) && stop("missing expression, perhaps wrap in {}?")
  if (!is.null(proc <- attr(daemons(), "daemons")) && proc) {

    arglist <- list(.expr = substitute(.expr), ...)
    envir <- list2env(arglist)
    ctx <- context(daemons())
    aio <- request(ctx, data = envir, send_mode = "serial", recv_mode = "serial", keep.raw = FALSE)
    `attr<-`(.subset2(aio, "aio"), "ctx", ctx)
    `class<-`(aio, c("mirai", "recvAio"))

  } else {

    arglist <- list(.expr = substitute(.expr), ...)
    envir <- list2env(arglist)
    url <- switch(.miraisysname,
                  Linux = sprintf("abstract://n%.15f", runif(1L)),
                  sprintf("ipc:///tmp/n%.15f", runif(1L)))
    arg <- c("--vanilla", "-e", shQuote(sprintf("mirai:::.(%s)", deparse(url))))
    cmd <- switch(.miraisysname,
                  Windows = file.path(R.home("bin"), "Rscript.exe"),
                  file.path(R.home("bin"), "Rscript"))
    system2(command = cmd, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
    sock <- socket(protocol = "req", listen = url, autostart = TRUE)
    ctx <- context(sock)
    aio <- request(ctx, data = envir, send_mode = "serial", recv_mode = "serial", keep.raw = FALSE)
    `attr<-`(.subset2(aio, "aio"), "ctx", ctx)
    `attr<-`(.subset2(aio, "aio"), "sock", sock)
    `class<-`(aio, c("mirai", "recvAio"))

  }

}

#' @rdname eval_mirai
#' @export
#'
mirai <- eval_mirai

#' mirai (Call Value)
#'
#' Call the value of a 'mirai', waiting for the the asynchronous operation to
#'     resolve if it is still in progress.
#'
#' @param mirai a 'mirai' object.
#'
#' @return The passed 'mirai' (invisibly). The retrieved value is stored in
#'     \code{$data}.
#'
#' @details This function will wait for the async operation to complete if still
#'     in progress (blocking).
#'
#'     If an error occured in evaluation, a nul byte \code{00} (or serialized
#'     nul byte) will be returned. \code{\link{is_nul_byte}} can be used to test
#'     for a nul byte.
#'
#'     The 'mirai' updates itself in place, so to access the value of a 'mirai'
#'     \code{x} directly, use \code{call_mirai(x)$data}.
#'
#' @section Alternatively:
#'
#'     The value of a 'mirai' may be accessed at any time at \code{$data}, and
#'     if yet to resolve, an 'unresolved' logical NA will be returned instead.
#'
#'     \code{\link{unresolved}} may also be used on a 'mirai', and returns TRUE
#'     only if a 'mirai' has yet to resolve and FALSE otherwise. This is suitable
#'     for use in control flow statements such as \code{while} or \code{if}.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- mirai(x + y + 1, x = 2, y = 3)
#' m
#' m$data
#' Sys.sleep(0.2)
#' m$data
#'
#' m <- mirai(as.matrix(df), df = data.frame())
#' call_mirai(m)$data
#'
#' m <- mirai({
#'   res <- rnorm(n)
#'   res / rev(res)
#' }, n = 1e6)
#' while (unresolved(m)) {
#'   cat("unresolved\n")
#'   Sys.sleep(0.1)
#' }
#' m$data
#'
#' }
#'
#' @export
#'
call_mirai <- function(mirai) call_aio(mirai)

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
#' @noRd
#'
.. <- function(.) {

  sock <- socket(protocol = "rep", dial = ., autostart = TRUE)
  on.exit(expr = {
    send(ctx, data = as.raw(0L), mode = "serial", echo = FALSE)
    close(sock)
    ..(.)
  })
  repeat {
    ctx <- context(sock)
    envir <- recv(ctx, mode = "serial", keep.raw = FALSE)
    missing(envir) && break
    msg <- eval(expr = .subset2(envir, ".expr"), envir = envir)
    send(ctx, data = msg, mode = "serial", echo = FALSE)
    close(ctx)
  }

  on.exit()
  close(sock)

}

#' mirai (Stop Evaluation)
#'
#' Stop evaluation of a mirai that is in progress.
#'
#' @param mirai a 'mirai' object.
#'
#' @return Invisible NULL.
#'
#' @details Stops the asynchronous operation associated with 'mirai' by aborting,
#'     and then waits for it to complete or to be completely aborted. The 'mirai'
#'     is then deallocated and attempting to access the value at \code{$data}
#'     will result in an error.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- mirai(Sys.sleep(n), n = 5)
#' stop_mirai(m)
#'
#' }
#'
#' @export
#'
stop_mirai <- function(mirai) stop_aio(mirai)

#' Is mirai
#'
#' Is the object a mirai.
#'
#' @param x an object.
#'
#' @return Logical value TRUE or FALSE.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' df <- data.frame()
#' m <- mirai(as.matrix(df), df = df)
#' is_mirai(m)
#' is_mirai(df)
#'
#' }
#'
#' @export
#'
is_mirai <- function(x) inherits(x, "mirai")

#' daemons (Background Processes)
#'
#' Set or view the number of daemons (background processes). Create persistent
#'     background processes to send \code{\link{mirai}} requests. Setting a
#'     positive number of daemons provides a potentially more efficient solution
#'     for async operations as new processes no longer need to be created on an
#'     ad hoc basis.
#'
#' @param ... an integer to set the number of daemons. 'view' to view the
#'     currently set number of daemons.
#'
#' @return Depending on the specified ... parameter:
#'     \itemize{
#'     \item{integer: integer change in number of daemons (created or destroyed).}
#'     \item{'view': integer number of currently set daemons.}
#'     \item{missing: the 'nanoSocket' for connecting to the daemons, or NULL if
#'     it is yet to be created.}
#'     }
#'
#' @details \{mirai\} will revert to the default behaviour of creating a new
#'     background process for each request if the number of daemons is set to 0.
#'
#'     The current implementation is low-level and ensures tasks are
#'     evenly-distributed amongst daemons without actively managing a task queue.
#'     This approach provides a robust and resource-light solution, particularly
#'     well-suited to working with similar-length tasks, or where the number of
#'     concurrent tasks typically does not exceed the number of available
#'     daemons.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' # To create 4 background processes
#' daemons(4)
#' # To view the number of background processes
#' daemons("view")
#' # To destroy them all
#' daemons(0)
#' }
#'
#' @export
#'
daemons <- function(...) {

  proc <- 0L
  url <- sock <- cmd <- arg <- NULL

  function(...) {

    if (missing(...)) {

      sock

    } else {

      identical(..1, "view") && return(if (is.null(d <- attr(sock, "daemons"))) 0L else d)
      is.numeric(..1) || stop("provide an integer to set daemons or 'view' to view daemons")
      set <- as.integer(..1)
      set >= 0L || stop("number of daemons must be zero or greater")
      delta <- set - proc
      delta == 0L && return(0L)

      if (is.null(url)) {
        url <<- switch(.miraisysname,
                      Linux = sprintf("abstract://n%.15f", runif(1L)),
                      sprintf("ipc:///tmp/n%.15f", runif(1L)))
        sock <<- socket(protocol = "req", listen = url, autostart = TRUE)
        arg <<- c("--vanilla", "-e", shQuote(sprintf("mirai:::..(%s)", deparse(url))))
        cmd <<- switch(.miraisysname,
                       Windows = file.path(R.home("bin"), "Rscript.exe"),
                       file.path(R.home("bin"), "Rscript"))
        reg.finalizer(sock, function(x) daemons(0L), onexit = TRUE)
      }

      if (delta > 0L) {
        orig <- proc
        for (i in seq_len(delta)) {
          system2(command = cmd, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
          proc <<- proc + 1L
        }
        attr(sock, "daemons") <- proc
        proc - orig

      } else {
        halt <- 0L
        for (i in seq_len(-delta)) {
          ctx <- context(sock)
          res <- send_aio(ctx, data = .mirai_scm(), mode = "serial", timeout = 2000L)
          if (.subset2(call_aio(res), "result")) {
            warning(sprintf("daemon %d shutdown failed", i))
          } else {
            halt <- halt - 1L
            proc <<- proc - 1L
          }
          close(ctx)
        }
        attr(sock, "daemons") <- proc
        halt

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

