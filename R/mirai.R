# mirai ------------------------------------------------------------------------

#' mirai Server (Async Executor)
#'
#' Implements an executor/server for the remote process. Awaits data, evaluates
#'     an expression in an environment containing the supplied data, and returns
#'     the result to the caller/client.
#'
#' @param url the internally assigned unique URL.
#'
#' @return Integer exit code.
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
#' @param ... named arguments specifying the variables contained in '.expr'.
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

  if (mirai(, 0L)) {

    arglist <- list(.expr = substitute(.expr), ...)
    envir <- list2env(arglist)
    ctx <- context(mirai())
    aio <- request(ctx, data = envir, send_mode = "serial", recv_mode = "serial", keep.raw = FALSE)
    mirai <- `class<-`(new.env(), "mirai")
    mirai[["aio"]] <- aio
    mirai[["context"]] <- ctx
    mirai

  } else {

    arglist <- list(.expr = substitute(.expr), ...)
    envir <- list2env(arglist)
    platform <- .subset2(.Platform, "OS.type")
    cmd <- switch(platform,
                  unix = file.path(R.home("bin"), "Rscript"),
                  windows = file.path(R.home("bin"), "Rscript.exe"))
    url <- sprintf("ipc:///tmp/n%.15f", runif(1L))
    arg <- c("--vanilla", "-e", shQuote(sprintf("mirai::exec(%s)", deparse(url))))
    system2(command = cmd, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
    sock <- socket(protocol = "req", listen = url)
    ctx <- context(sock)
    aio <- request(ctx, data = envir, send_mode = "serial", recv_mode = "serial", keep.raw = FALSE)
    mirai <- `class<-`(new.env(), "mirai")
    mirai[["aio"]] <- aio
    mirai[["socket"]] <- sock
    mirai

  }

}

#' Call mirai (Retrieve Value)
#'
#' Retrieve the value of a mirai (optionally waiting for the the asynchronous
#'     operation to resolve if it is still in progress).
#'
#' @param mirai a 'mirai' object.
#' @param wait [default TRUE] whether to wait for completion of the asynschronous
#'     operation (blocking) or else return immediately.
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

  if (!is.null(aio <- .subset2(mirai, "aio"))) {
    if (!missing(wait) && !isTRUE(wait)) {
      is.null(call_aio(aio, block = FALSE)) && return()
    } else {
      call_aio(aio)
    }
    if (is.null(.subset2(mirai, "socket"))) {
      close(.subset2(mirai, "context"))
      rm("context", envir = mirai)
    } else {
      close(.subset2(mirai, "socket"))
      rm("socket", envir = mirai)
    }
    mirai[["value"]] <- .subset2(aio, "data")
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

#' mirai Server (Async Execution Daemon)
#'
#' Implements a persistent executor/server for the remote process. Awaits data,
#'     evaluates an expression in an environment containing the supplied data,
#'     and returns the result to the caller/client.
#'
#' @inheritParams exec
#'
#' @return Integer exit code.
#'
#' @keywords internal
#' @export
#'
daemon <- function(url) {

  sock <- socket(protocol = "rep", dial = url)
  ctx <- context(sock)
  on.exit(expr = {
    send_aio(ctx, data = as.raw(0L), mode = "serial")
    close(sock)
    daemon(url)
  })
  while (TRUE) {
    envir <- recv_ctx(ctx, mode = "serial", keep.raw = FALSE)
    missing(envir) && break
    msg <- eval(expr = .subset2(envir, ".expr"), envir = envir)
    send_ctx(ctx, data = msg, mode = "serial", echo = FALSE)
  }

  on.exit()
  send_aio(ctx, data = as.raw(1L), mode = "serial")
  close(sock)

}

#' mirai (Daemon Manager)
#'
#' Set or view the number of daemons (background processes). Use this function
#'     to create persistent background processes to send \code{\link{eval_mirai}}
#'     requests. Setting
#'     a positive number of daemons provides a more efficient solution for async
#'     operations as new processes do not need to be spun up on an ad hoc basis.
#'     [Experimental]
#'
#' @param set_daemons integer number of background processes.
#' @param view_daemons without specifying 'set_daemons', specify any value to
#'     view the number of currently active background processes e.g. \code{mirai(, 0)}.
#'
#' @return Without specifying any arguments, the 'nanoSocket' for connecting to
#'     the background processes, or NULL if it has yet to be created. When
#'     specifying 'set_daemons', the return value will depend on whether
#'     background processes are created or destroyed (see details section). If
#'     'set_daemons' is left blank but 'view_daemons' is specified, the integer
#'     number of currently active daemons.
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
#'     Reverts to the default behaviour of creating a new background process for
#'     each request if the number of daemons is set to 0.
#'
#'     Implementation note: uses the scalability protocols from the NNG library
#'     to provide massively-scalable load-balancing. \code{\link{eval_mirai}}
#'     requests are automatically routed to the optimal node based on
#'     back-pressure and then in a round-robin fashion.
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
#' # To view the number of active background processes
#' mirai(, 0)
#' # To destroy them all
#' mirai(0)
#' }
#'
#' @export
#'
mirai <- function(...) {

  daemons <- 0L
  url <- sock <- cmd <- arg <- NULL

  function(set_daemons, view_daemons) {
    if (missing(set_daemons)) {

      if (missing(view_daemons)) sock else daemons

    } else {

      set_daemons <- as.integer(set_daemons)
      set_daemons >= 0L || stop("number of daemons must be zero or greater")
      if (is.null(url)) url <<- sprintf("ipc:///tmp/n%.15f", runif(1L))
      if (is.null(sock)) sock <<- socket(protocol = "req", listen = url)
      if (is.null(cmd)) cmd <<- switch(.subset2(.Platform, "OS.type"),
                                      unix = file.path(R.home("bin"), "Rscript"),
                                      windows = file.path(R.home("bin"), "Rscript.exe"))
      if (is.null(arg)) arg <<- c("--vanilla", "-e", shQuote(sprintf("mirai::daemon(%s)", deparse(url))))

      delta <- set_daemons - daemons

      if (delta > 0L) {
        created <- 0L
        for (i in seq_len(delta)) {
          system2(command = cmd, args = arg, stdout = NULL, stderr = NULL, wait = FALSE)
          created <- created + 1L
        }
        daemons <<- daemons + created
        created

      } else if (delta < 0L) {
        res <- vector(mode = "list", length = -delta)
        for (i in seq_len(-delta)) {
          ctx <- context(sock)
          aio <- request(ctx, data = .mirai_scm(), send_mode = "serial", recv_mode = "serial", keep.raw = FALSE)
          call_aio(aio, block = TRUE)
          close(ctx)
          if (!identical(aio$data, as.raw(1L))) message("failed to destroy process ", i)
          res[[i]] <- aio$data
          daemons <<- daemons - 1L
        }
        res

      } else {
        invisible()
      }

    }

  }

}

