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

# internals --------------------------------------------------------------------

parse_dots <- function(...)
  if (missing(...)) "" else {
    dots <- list(...)
    for (dot in dots)
      is.numeric(dot) || is.logical(dot) || stop(.messages[["wrong_dots"]])
    dnames <- names(dots)
    dots <- strcat(",", paste(dnames, dots, sep = "=", collapse = ","))
    "output" %in% dnames && return(`class<-`(dots, "output"))
    dots
  }

parse_tls <- function(tls)
  switch(length(tls) + 1L,
         "",
         sprintf(",tls='%s'", tls),
         sprintf(",tls=c('%s','%s')", tls[1L], tls[2L]))

parse_cleanup <- function(cleanup)
  c(cleanup %% 2L, (clr <- as.raw(cleanup)) & as.raw(2L), clr & as.raw(4L), clr & as.raw(8L))

process_url <- function(url, .compute) {
  if (is.numeric(url)) {
    vec <- ..[[.compute]][["urls"]]
    is.null(vec) && stop(.messages[["daemons_unset"]])
    all(url >= 1L, url <= length(vec)) || stop(.messages[["url_spec"]])
    url <- vec[url]
  } else {
    lapply(url, parse_url)
  }
  url
}

write_args <- function(dots, rs = NULL, tls = NULL, libpath = NULL)
  shQuote(switch(length(dots),
                 sprintf("mirai::.daemon('%s')", dots[[1L]]),
                 sprintf("mirai::daemon('%s'%s%s)", dots[[1L]], dots[[2L]], parse_tls(tls)),
                 sprintf("mirai::daemon('%s'%s%s,rs=c(%s))", dots[[1L]], dots[[2L]], parse_tls(tls), paste0(dots[[3L]], collapse = ",")),
                 sprintf(".libPaths(c('%s',.libPaths()));mirai::dispatcher('%s',n=%d,rs=c(%s),monitor='%s'%s)", libpath, dots[[1L]], dots[[3L]], paste0(rs, collapse= ","), dots[[4L]], dots[[2L]]),
                 sprintf(".libPaths(c('%s',.libPaths()));mirai::dispatcher('%s',c('%s'),n=%d,monitor='%s'%s)", libpath, dots[[1L]], paste0(dots[[3L]], collapse = "','"), dots[[4L]], dots[[5L]], dots[[2L]])))

launch_daemon <- function(..., rs = NULL, tls = NULL) {
  dots <- list(...)
  dlen <- length(dots)
  output <- dlen > 1L && is.object(dots[[2L]])
  libpath <- if (dlen > 3L) (lp <- .libPaths())[file.exists(file.path(lp, "mirai"))][1L]
  system2(command = .command, args = c(if (length(libpath)) "--vanilla", "-e", write_args(dots, rs = rs, tls = tls, libpath = libpath)), stdout = if (output) "", stderr = if (output) "", wait = FALSE)
}

launch_and_sync_daemon <- function(sock, ..., rs = NULL, tls = NULL, pass = NULL) {
  cv <- cv()
  pipe_notify(sock, cv = cv, add = TRUE, remove = FALSE, flag = TRUE)
  if (is.character(tls)) {
    switch(
      length(tls),
      {
        on.exit(Sys.unsetenv("MIRAI_TEMP_FIELD1"))
        Sys.setenv(MIRAI_TEMP_FIELD1 = tls)
        Sys.unsetenv("MIRAI_TEMP_FIELD2")
      },
      {
        on.exit(Sys.unsetenv(c("MIRAI_TEMP_FIELD1", "MIRAI_TEMP_FIELD2")))
        Sys.setenv(MIRAI_TEMP_FIELD1 = tls[1L])
        Sys.setenv(MIRAI_TEMP_FIELD2 = tls[2L])
      }
    )
    if (is.character(pass)) {
      on.exit(Sys.unsetenv("MIRAI_TEMP_VAR"), add = TRUE)
      Sys.setenv(MIRAI_TEMP_VAR = pass)
    }
  }
  launch_daemon(..., rs = rs)
  until(cv, .timelimit) && stop(if (...length() < 3L) .messages[["sync_timeout"]] else .messages[["sync_dispatch"]])
}

dial_and_sync_socket <- function(sock, url, asyncdial, tls = NULL) {
  cv <- cv()
  if (length(tls) && !asyncdial) {
    pipe_notify(sock, cv = cv, add = TRUE, remove = FALSE, flag = TRUE)
    dial(sock, url = url, autostart = TRUE, tls = tls, error = TRUE)
    until(cv, .timelimit) && stop(.messages[["sync_timeout"]])
  } else {
    pipe_notify(sock, cv = cv, add = TRUE, remove = FALSE, flag = FALSE)
    dial(sock, url = url, autostart = length(tls) || asyncdial || NA, tls = tls, error = TRUE)
    wait(cv)
  }
}

sub_real_port <- function(port, url) sub("(?<=:)0(?![^/])", port, url, perl = TRUE)

auto_tokenized_url <- function() strcat(.urlscheme, random(12L))

new_tokenized_url <- function(url) sprintf("%s/%s", url, random(12L))

req_socket <- function(url, tls = NULL, resend = .intmax)
  `opt<-`(socket(protocol = "req", listen = url, tls = tls), "req:resend-time", resend)

query_dispatcher <- function(sock, command, mode) {
  send(sock, data = command, mode = 2L, block = .timelimit)
  recv(sock, mode = mode, block = .timelimit)
}

query_status <- function(envir) {
  res <- query_dispatcher(sock = envir[["sockc"]], command = 0L, mode = 5L)
  is.object(res) && return(res)
  `attributes<-`(res, list(dim = c(envir[["n"]], 5L),
                           dimnames = list(envir[["urls"]], c("i", "online", "instance", "assigned", "complete"))))
}

init_monitor <- function(sockc, envir) {
  res <- query_dispatcher(sockc, command = 0L, mode = 2L)
  is.object(res) && stop(.messages[["sync_timeout"]])
  `[[<-`(`[[<-`(`[[<-`(envir, "sockc", sockc), "urls", res[-1L]), "pid", as.integer(res[1L]))
}

create_stream <- function(n, seed, envir) {
  rexp(n = 1L)
  oseed <- .GlobalEnv[[".Random.seed"]]
  RNGkind("L'Ecuyer-CMRG")
  if (length(seed)) set.seed(seed)
  `[[<-`(envir, "stream", .GlobalEnv[[".Random.seed"]])
  `[[<-`(.GlobalEnv, ".Random.seed", oseed)
}

next_stream <- function(envir) {
  stream <- envir[["stream"]]
  length(stream) || return()
  `[[<-`(envir, "stream", nextRNGStream(stream))
  stream
}

get_and_reset_env <- function(x) {
  candidate <- Sys.getenv(x)
  if (nzchar(candidate)) {
    Sys.unsetenv(x)
    candidate
  }
}

parse_check_local_url <- function(url) {
  purl <- parse_url(url)
  purl[["hostname"]] %in% c("localhost", "127.0.0.1") || stop(.messages[["requires_local"]])
  purl
}

find_dot <- function(args) {
  sel <- args == "."
  any(sel) || stop(.messages[["dot_required"]])
  sel
}

mk_interrupt_error <- function(e) `class<-`("", c("miraiInterrupt", "errorValue"))

mk_mirai_error <- function(e) {
  x <- .subset2(e, "call")
  call <- if (length(x)) deparse(x, width.cutoff = 500L, backtick = TRUE, control = NULL, nlines = 1L)
  msg <- if (is.null(call) || call == "eval(expr = ._mirai_.[[\".expr\"]], envir = ._mirai_., enclos = NULL)")
    sprintf("Error: %s", .subset2(e, "message")) else
      sprintf("Error in %s: %s", call, .subset2(e, "message"))
  cat(strcat(msg, "\n"), file = stderr());
  `class<-`(msg, c("miraiError", "errorValue", "try-error"))
}

# nocov start
# tested manually in tests/parallel/parallel-tests.R

node_unresolved <- function(node) unresolved(.subset2(node, "mirai"))

# nocov end
