
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mirai <a href="https://shikokuchuo.net/mirai/" alt="mirai"><img src="man/figures/logo.png" alt="mirai logo" align="right" width="120"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/mirai?color=112d4e)](https://CRAN.R-project.org/package=mirai)
[![mirai status
badge](https://shikokuchuo.r-universe.dev/badges/mirai?color=ddcacc)](https://shikokuchuo.r-universe.dev)
[![R-CMD-check](https://github.com/shikokuchuo/mirai/workflows/R-CMD-check/badge.svg)](https://github.com/shikokuchuo/mirai/actions)
[![codecov](https://codecov.io/gh/shikokuchuo/mirai/branch/main/graph/badge.svg)](https://app.codecov.io/gh/shikokuchuo/mirai)
<!-- badges: end -->

Minimalist async evaluation framework for R.

Lightweight parallel code execution, local or distributed across the
network.

Designed for simplicity, a ‘mirai’ evaluates an arbitrary expression
asynchronously, resolving automatically upon completion.

Built on ‘nanonext’ and ‘NNG’ (Nanomsg Next Gen), uses scalability
protocols not subject to R connection limits and transports faster than
TCP/IP where applicable.

`mirai()` returns a ‘mirai’ object immediately. ‘mirai’ (未来 みらい) is
Japanese for ‘future’.

The asynchronous ‘mirai’ task runs in an ephemeral or persistent
process, spawned locally or distributed across the network.

{mirai} has a tiny pure R code base, relying solely on {nanonext}, a
high-performance binding for the ‘NNG’ (Nanomsg Next Gen) C library with
zero package dependencies.

### Table of Contents

1.  [Installation](#installation)
2.  [Example 1: Compute-intensive
    Operations](#example-1-compute-intensive-operations)
3.  [Example 2: I/O-bound Operations](#example-2-io-bound-operations)
4.  [Example 3: Resilient Pipelines](#example-3-resilient-pipelines)
5.  [Daemons: Local Persistent
    Processes](#daemons-local-persistent-processes)
6.  [Distributed Computing: Remote
    Servers](#distributed-computing-remote-servers)
7.  [Compute Profiles](#compute-profiles)
8.  [Errors, Interrupts and Timeouts](#errors-interrupts-and-timeouts)
9.  [Deferred Evaluation Pipe](#deferred-evaluation-pipe)
10. [Links](#links)

### Installation

Install the latest release from CRAN:

``` r
install.packages("mirai")
```

or the development version from rOpenSci R-universe:

``` r
install.packages("mirai", repos = "https://shikokuchuo.r-universe.dev")
```

[« Back to ToC](#table-of-contents)

### Example 1: Compute-intensive Operations

Use case: minimise execution times by performing long-running tasks
concurrently in separate processes.

Multiple long computes (model fits etc.) can be performed in parallel on
available computing cores.

Use `mirai()` to evaluate an expression asynchronously in a separate,
clean R process.

A ‘mirai’ object is returned immediately.

``` r
library(mirai)

m <- mirai({
  res <- rnorm(n) + m
  res / rev(res)
}, n = 1e8, m = runif(1))

m
#> < mirai >
#>  - $data for evaluated result
```

Above, all named objects are passed through to the mirai.

The ‘mirai’ yields an ‘unresolved’ logical NA value whilst the async
operation is ongoing.

``` r
m$data
#> 'unresolved' logi NA
```

Upon completion, the ‘mirai’ resolves automatically to the evaluated
result.

``` r
m$data |> str()
#>  num [1:100000000] -0.3313 1.6611 0.7669 -0.0476 3.3869 ...
```

Alternatively, explicitly call and wait for the result using
`call_mirai()`.

``` r
call_mirai(m)$data |> str()
#>  num [1:100000000] -0.3313 1.6611 0.7669 -0.0476 3.3869 ...
```

[« Back to ToC](#table-of-contents)

### Example 2: I/O-bound Operations

Use case: ensure execution flow of the main process is not blocked.

High-frequency real-time data cannot be written to file/database
synchronously without disrupting the execution flow.

Cache data in memory and use `mirai()` to perform periodic write
operations concurrently in a separate process.

A ‘mirai’ object is returned immediately.

Below, ‘.args’ accepts a list of objects already present in the calling
environment to be passed to the mirai.

``` r
library(mirai)

x <- rnorm(1e6)
file <- tempfile()

m <- mirai(write.csv(x, file = file), .args = list(x, file))
```

`unresolved()` may be used in control flow statements to perform actions
which depend on resolution of the ‘mirai’, both before and after.

This means there is no need to actually wait (block) for a ‘mirai’ to
resolve, as the example below demonstrates.

``` r
# unresolved() queries for resolution itself so no need to use it again within the while loop

while (unresolved(m)) {
  cat("while unresolved\n")
  Sys.sleep(0.5)
}
#> while unresolved
#> while unresolved

cat("Write complete:", is.null(m$data))
#> Write complete: TRUE
```

Now actions which depend on the resolution may be processed, for example
the next write.

[« Back to ToC](#table-of-contents)

### Example 3: Resilient Pipelines

Use case: isolating code that can potentially fail in a separate process
to ensure continued uptime.

As part of a data science or machine learning pipeline, iterations of
model training may periodically fail for stochastic and uncontrollable
reasons (e.g. buggy memory management on graphics cards). Running each
iteration in a ‘mirai’ process isolates this potentially-problematic
code such that if it does fail, it does not crash the entire pipeline.

``` r
library(mirai)

run_iteration <- function(i) {
  
  if (runif(1) < 0.15) stop("random error", call. = FALSE) # simulates a stochastic error rate
  sprintf("iteration %d successful", i)
  
}

for (i in 1:10) {
  
  m <- mirai(run_iteration(i), .args = list(run_iteration, i))
  while (is_error_value(call_mirai(m)$data)) {
    cat(m$data, "\n")
    m <- mirai(run_iteration(i), .args = list(run_iteration, i))
  }
  cat(m$data, "\n")
  
}
#> iteration 1 successful 
#> iteration 2 successful 
#> iteration 3 successful 
#> iteration 4 successful 
#> iteration 5 successful 
#> iteration 6 successful 
#> Error: random error 
#> iteration 7 successful 
#> iteration 8 successful 
#> iteration 9 successful 
#> iteration 10 successful
```

Further, by testing the return value of each ‘mirai’ for errors,
error-handling code is then able to automate recovery and re-attempts,
as in the above example. Further details on [error
handling](#errors-interrupts-and-timeouts) can be found in the section
below.

The end result is a resilient and fault-tolerant pipeline that minimises
downtime by eliminating interruptions of long computes.

[« Back to ToC](#table-of-contents)

### Daemons: Local Persistent Processes

Daemons, or persistent background processes, may be set to receive
‘mirai’ requests.

This is potentially more efficient as new processes no longer need to be
created on an *ad hoc* basis.

#### With Dispatcher (default)

Call `daemons()` specifying the number of daemons to launch.

``` r
daemons(6)
#> [1] 6
```

To view the current status, call `daemons()` with no arguments. This
provides the number of active connections along with a matrix of
statistics for each daemon.

``` r
daemons()
#> $connections
#> [1] 1
#> 
#> $daemons
#>                        status_online status_busy tasks_assigned tasks_complete
#> abstract://n3167963634             1           0              0              0
#> abstract://n3016422522             1           0              0              0
#> abstract://n2909713717             1           0              0              0
#> abstract://n987066846              1           0              0              0
#> abstract://n1395514703             1           0              0              0
#> abstract://n3999618829             1           0              0              0
#>                        instance #
#> abstract://n3167963634          1
#> abstract://n3016422522          1
#> abstract://n2909713717          1
#> abstract://n987066846           1
#> abstract://n1395514703          1
#> abstract://n3999618829          1
```

The default `dispatcher = TRUE` launches a `dispatcher()` background
process that connects to individual background `server()` processes on
the local machine. This ensures that tasks are dispatched efficiently on
a FIFO basis to servers for processing. Tasks are queued at the
dispatcher and only sent to servers that can begin immediate execution
of the task.

A dispatcher running local daemons is self-repairing if one of the
daemons crashes or is terminated.

``` r
daemons(0)
#> [1] 0
```

Set the number of daemons to zero to reset. This reverts to the default
of creating a new background process for each ‘mirai’ request.

#### Without Dispatcher

Alternatively, specifying `dispatcher = FALSE`, the background daemon
processes connect directly to the client.

``` r
daemons(6, dispatcher = FALSE)
#> [1] 6
```

Requesting the status now shows 6 connections and 6 daemons.

``` r
daemons()
#> $connections
#> [1] 6
#> 
#> $daemons
#> [1] 6
```

This implementation ensures that tasks are evenly-distributed amongst
daemons, however requires tasks to be sent to daemons immediately. This
means that optimal scheduling is not guaranteed as the duration of tasks
cannot be known *a priori*. As an example, tasks could be queued at a
server behind a long-running task, whilst other servers remain idle.

The advantage of this approach is that it is low-level and does not
require an additional dispatcher process. It is well-suited to working
with similar-length tasks, or where the number of concurrent tasks
typically does not exceed available daemons.

``` r
daemons(0)
#> [1] 0
```

Set the number of daemons to zero to reset.

[« Back to ToC](#table-of-contents)

### Distributed Computing: Remote Servers

The daemons interface may also be used to send tasks for computation to
server processes on the network.

Call `daemons()` specifying ‘url’ as a character string the client
network address and a port that is able to accept incoming connections.

The examples below use an illustrative local network IP address of
‘192.168.0.2’. A port on the client also needs to be open and available
for inbound connections from the local network, illustratively ‘5555’ in
the examples below.

#### Connecting to Remote Servers Through Dispatcher

The default `dispatcher = TRUE` creates a background `dispatcher()`
process on the local client machine, which listens to a vector of URLs
that remote servers dial in to, with each server having its unique URL.

It is recommended to use a websocket URL starting `ws://` instead of TCP
in this scenario (used interchangeably with `tcp://`). A websocket URL
supports a path after the port number, which can be made unique for each
server. In this way a dispatcher can connect to an arbitrary number of
servers over a single port.

``` r
# daemons(n = 4, url = "ws://192.168.0.2:5555")

daemons(n = 4, url = "ws://:5555")
#> [1] 4
```

Above, a single URL was supplied, along with `n = 4` to specify that the
dispatcher should listen at 4 URLs. In such a case, an integer sequence
is automatically appended to the path `/1` through `/4` to produce these
URLs.

Alternatively, supplying a vector of URLs allows the use of arbitrary
port numbers / paths, e.g.:

``` r
# daemons(url = c("ws://:5555/cpu", "ws://:5555/gpu", "ws://:12560", "ws://:12560/2"))
```

Above, ‘n’ is not specified, in which case its value is inferred from
the length of the ‘url’ vector supplied.

–

On the remote resource, `server()` may be called from an R session, or
directly from a shell using Rscript. Each server instance should dial
into one of the unique URLs that the dispatcher is listening to:

    Rscript -e 'mirai::server("ws://192.168.0.2:5555/1")'
    Rscript -e 'mirai::server("ws://192.168.0.2:5555/2")'
    Rscript -e 'mirai::server("ws://192.168.0.2:5555/3")'
    Rscript -e 'mirai::server("ws://192.168.0.2:5555/4")'

–

Requesting status, on the client:

``` r
daemons()
#> $connections
#> [1] 1
#> 
#> $daemons
#>              status_online status_busy tasks_assigned tasks_complete instance #
#> ws://:5555/1             1           0              0              0          1
#> ws://:5555/2             1           0              0              0          1
#> ws://:5555/3             1           0              0              0          1
#> ws://:5555/4             1           0              0              0          1
```

As per the local case, `$connections` will show the single connection to
the dispatcher process, however `$daemons` will provide the matrix of
statistics for the remote servers.

`status_online` shows as 1 when there is an active connection, or else 0
if a server has yet to connect or has disconnected.

`status_busy` will be 1 if the server is currently processing a task, or
else 0.

`tasks_assinged` shows the cumulative number of tasks assigned to the
server instance by the dispatcher.

`tasks_complete` shows the cumulative number of tasks completed by the
server instance.

`instance #` will increment by 1 every time there is a new connection at
a URL. When this happens, the `tasks_assigned` and `tasks_complete`
statistics will also reset. This is designed to track new server
instances connecting after previous ones have ended (due to time-outs
etc.).

The dispatcher will automatically adjust to the number of servers
actually connected. Hence it is possible to dynamically scale up or down
the number of servers according to requirements (limited to the ‘n’ URLs
assigned to the dispatcher).

To reset all connections and revert to default behaviour:

``` r
daemons(0)
#> [1] 0
```

This also sends an exit signal to the dispatcher and all connected
servers so that they exit automatically.

#### Connecting to Remote Servers Directly

By specifying `dispatcher = FALSE`, remote servers can connect directly
to the client.

``` r
daemons(url = "tcp://192.168.0.2:5555", dispatcher = FALSE)
```

Alternatively, simply supply a colon followed by the port number to
listen on all interfaces on the local host, for example:

``` r
daemons(url = "tcp://:5555", dispatcher = FALSE)
#> [1] "tcp://:5555"
```

Here, `dispatcher = FALSE` is specified so that servers connect directly
to the client. The network topology is such that the client listens at
the above address, and distributes tasks to all connected server
processes.

–

On the server, `server()` may be called from an R session, or an Rscript
invocation from a shell. This sets up a remote daemon process that
connects to the client URL and receives tasks:

    Rscript -e 'mirai::server("tcp://192.168.0.2:5555")'

–

On the client, requesting the status will return the client URL for
`daemons`. The number of daemons connecting to this URL is not limited
and network resources may be added and removed at any time, with tasks
automatically distributed to all server processes.

`connections` will show the actual number of connected server instances
(2 in the example below).

``` r
daemons()
#> $connections
#> [1] 2
#> 
#> $daemons
#> [1] "tcp://:5555"
```

To reset all connections and revert to default behaviour:

``` r
daemons(0)
#> [1] 0
```

This also sends an exit signal to connected server instances so that
they exit automatically.

[« Back to ToC](#table-of-contents)

### Compute Profiles

The `daemons` interface allows the easy specification of compute
profiles. This is for managing tasks with heterogeneous compute
requirements:

- send tasks to different servers or server clusters with the
  appropriate specifications (in terms of CPUs / memory / GPU /
  accelerators etc.)
- split tasks between local and remote computation

Simply specify the argument `.compute` when calling `daemons()` with a
profile name (which is ‘default’ for the default profile). The daemons
settings are saved under the named profile.

To launch a ‘mirai’ task using a specific compute profile, specify the
‘.compute’ argument to `mirai()`, which defaults to the ‘default’
compute profile.

[« Back to ToC](#table-of-contents)

### Errors, Interrupts and Timeouts

If execution in a mirai fails, the error message is returned as a
character string of class ‘miraiError’ and ‘errorValue’ to facilitate
debugging. `is_mirai_error()` can be used to test for mirai execution
errors.

``` r
m1 <- mirai(stop("occurred with a custom message", call. = FALSE))
call_mirai(m1)$data
#> 'miraiError' chr Error: occurred with a custom message

m2 <- mirai(mirai::mirai())
call_mirai(m2)$data
#> 'miraiError' chr Error in mirai::mirai(): missing expression, perhaps wrap in {}?

is_mirai_error(m2$data)
#> [1] TRUE
is_error_value(m2$data)
#> [1] TRUE
```

If during a `call_mirai()` an interrupt e.g. ctrl+c is sent, the mirai
will resolve to an empty character string of class ‘miraiInterrupt’ and
‘errorValue’. `is_mirai_interrupt()` may be used to test for such
interrupts.

``` r
is_mirai_interrupt(m2$data)
#> [1] FALSE
```

If execution of a mirai surpasses the timeout set via the ‘.timeout’
argument, the mirai will resolve to an ‘errorValue’. This can, amongst
other things, guard against mirai processes that hang and never return.

``` r
m3 <- mirai(nanonext::msleep(1000), .timeout = 500)
call_mirai(m3)$data
#> 'errorValue' int 5 | Timed out

is_mirai_error(m3$data)
#> [1] FALSE
is_mirai_interrupt(m3$data)
#> [1] FALSE
is_error_value(m3$data)
#> [1] TRUE
```

`is_error_value()` tests for all mirai execution errors, user interrupts
and timeouts.

[« Back to ToC](#table-of-contents)

### Deferred Evaluation Pipe

{mirai} implements a deferred evaluation pipe `%>>%` for working with
potentially unresolved values.

Pipe a mirai `$data` value forward into a function or series of
functions and it initially returns an ‘unresolvedExpr’.

The result may be queried at `$data`, which will return another
‘unresolvedExpr’ whilst unresolved. However when the original value
resolves, the ‘unresolvedExpr’ will simultaneously resolve into a
‘resolvedExpr’, for which the evaluated result will be available at
`$data`.

It is possible to use `unresolved()` around a ‘unresolvedExpr’ or its
`$data` element to test for resolution, as in the example below.

The pipe operator semantics are similar to R’s base pipe `|>`:

`x %>>% f` is equivalent to `f(x)` <br /> `x %>>% f()` is equivalent to
`f(x)` <br /> `x %>>% f(y)` is equivalent to `f(x, y)`

``` r
m <- mirai({nanonext::msleep(500); 1})
b <- m$data %>>% c(2, 3) %>>% as.character()
b
#> < unresolvedExpr >
#>  - $data to query resolution
b$data
#> < unresolvedExpr >
#>  - $data to query resolution
nanonext::msleep(800)
b$data
#> [1] "1" "2" "3"
b
#> < resolvedExpr: $data >
```

[« Back to ToC](#table-of-contents)

### Links

{mirai} website: <https://shikokuchuo.net/mirai/><br /> {mirai} on CRAN:
<https://cran.r-project.org/package=mirai>

Listed in CRAN Task View: <br /> - High Performance Computing:
<https://cran.r-project.org/view=HighPerformanceComputing>

{nanonext} website: <https://shikokuchuo.net/nanonext/><br /> {nanonext}
on CRAN: <https://cran.r-project.org/package=nanonext>

NNG website: <https://nng.nanomsg.org/><br />

[« Back to ToC](#table-of-contents)

–

Please note that this project is released with a [Contributor Code of
Conduct](https://shikokuchuo.net/mirai/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
