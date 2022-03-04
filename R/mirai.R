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

  missing(.) && stop("this function is for package internal use only")
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
#'     if yet to resolve, an 'unresolved' logical NA value will be returned
#'     instead.
#'
#'     \code{\link{unresolved}} may also be used, which returns TRUE only if a
#'     'mirai' has yet to resolve and FALSE otherwise. This is suitable for use
#'     in control flow statements such as \code{while} or \code{if}.
#'
#'     Alternatively, to call (and wait for) the result, use
#'     \code{\link{call_mirai}} on the returned 'mirai' object. This will block
#'     until the result is returned.
#'
#'     The expression '.expr' will be evaluated in a new R process in a clean
#'     environment consisting of the named objects passed as '...' only.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- eval_mirai(x + y + 1, x = 2, y = 3)
#' m
#' m$data
#' Sys.sleep(0.2)
#' m$data
#'
#' m <- eval_mirai(as.matrix(df), df = data.frame())
#' call_mirai(m)$data
#'
#' m <- eval_mirai({
#'   res <- rnorm(n)
#'   res / rev(res)
#' }, n = 1e6)
#' while(unresolved(m)) {
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
  if (mirai(view_daemons = TRUE)) {

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

#' mirai (Call Value)
#'
#' Call the value of a 'mirai' (waiting for the the asynchronous operation to
#'     resolve if it is still in progress).
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
#'     The 'mirai' updates itself in place, so do not assign the output of this
#'     function to avoid duplicates. To access the value of a 'mirai' \code{x}
#'     directly, use \code{call_mirai(x)$data}.
#'
#' @section Alternatively:
#'
#'     The value of a 'mirai' may be accessed at any time at \code{$data}, and
#'     if yet to resolve, an 'unresolved' logical NA value will be returned
#'     instead.
#'
#'     \code{\link{unresolved}} may also be used, which returns TRUE only if a
#'     'mirai' has yet to resolve and FALSE otherwise. This is suitable for use
#'     in control flow statements such as \code{while} or \code{if}.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' m <- eval_mirai(x + y + 1, x = 2, y = 3)
#' m
#' m$data
#' Sys.sleep(0.2)
#' m$data
#'
#' m <- eval_mirai(as.matrix(df), df = data.frame())
#' call_mirai(m)$data
#'
#' m <- eval_mirai({
#'   res <- rnorm(n)
#'   res / rev(res)
#' }, n = 1e6)
#' while(unresolved(m)) {
#'   cat("unresolved\n")
#'   Sys.sleep(0.1)
#' }
#' m$data
#'
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

  missing(.) && stop("this function is for package internal use only")
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
#' m <- eval_mirai(Sys.sleep(n), n = 5)
#' stop_mirai(m)
#'
#' }
#'
#' @export
#'
stop_mirai <- function(mirai) {

  if (!is.null(.subset2(mirai, "con"))) {
    stop_aio(mirai)
    close(.subset2(mirai, "con"))
    rm("con", envir = mirai)
  }
  invisible()

}

#' mirai (Control Panel)
#'
#' Settings that controls the overall behaviour of \{mirai\}. Note that as all
#'     arguments appear after '...', they must be specified explicitly and in
#'     full (positional and partial matching do not apply). Specify a single
#'     argument only: if more than one is specified, only the first will be
#'     taken into account, in the order they appear in the argument list below.
#'
#' @param ... reserved.
#' @param set_daemons set an integer number of background processes.
#' @param view_daemons specify any value to return the current number of
#'     background processes.
#'
#' @return Depending on specified parameters:
#'     \itemize{
#'     \item{'set_daemons': the return value will depend on whether background
#'     processes are created or destroyed (see Daemons section).}
#'     \item{'view_daemons': the integer number of background processes.}
#'     \item{none: the 'nanoSocket' for connecting to the background processes,
#'     or NULL if it has yet to be created.}
#'     }
#'
#' @section Daemons:
#'
#'     Set or view the number of daemons (background processes). Create
#'     persistent background processes to send \code{\link{eval_mirai}} requests.
#'     Setting a positive number of daemons provides a potentially more efficient
#'     solution for async operations as new processes do not need to be created
#'     on an ad hoc basis. [Experimental]
#'
#'     Background processes will be created or destroyed as appropriate.
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
#'
#' # To create 4 background processes
#' mirai(set_daemons = 4)
#' # To view the number of background processes
#' mirai(view_daemons = TRUE)
#' # To destroy them all
#' mirai(set_daemons = 0)
#' }
#'
#' @export
#'
mirai <- function(...) {

  daemons <- 0L
  url <- sock <- cmd <- arg <- NULL

  function(..., set_daemons, view_daemons) {

    if (missing(set_daemons)) {

      if (missing(view_daemons)) sock else daemons

    } else {

      set_daemons <- as.integer(set_daemons)
      set_daemons >= 0L || stop("number of daemons must be zero or greater")
      delta <- set_daemons - daemons
      delta == 0L && return(invisible())

      if (is.null(url)) {
        url <<- sprintf("ipc:///tmp/n%.15f", runif(1L))
        sock <<- socket(protocol = "req", listen = url)
        cmd <<- switch(.subset2(.Platform, "OS.type"),
                       unix = file.path(R.home("bin"), "Rscript"),
                       windows = file.path(R.home("bin"), "Rscript.exe"))
        arg <<- c("--vanilla", "-e", shQuote(sprintf("mirai::..(%s)", deparse(url))))
      }

      if (delta > 0L) {
        orig <- daemons
        for (i in seq_len(delta)) {
          system2(command = cmd, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
          daemons <<- daemons + 1L
        }
        daemons - orig

      } else {
        res <- vector(mode = "list", length = -delta)
        for (i in seq_len(-delta)) {
          ctx <- context(sock)
          aio <- request(ctx, data = .mirai_scm(), send_mode = "serial", recv_mode = "serial", keep.raw = FALSE)
          call_aio(aio)
          close(ctx)
          if (!identical(.subset2(aio, "data"), as.raw(1L))) message(Sys.time(), " [ sigterm fail ] daemon: ", i)
          res[[i]] <- .subset2(aio, "data")
          daemons <<- daemons - 1L
        }
        res

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

