
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
4.  [Daemons](#daemons)
5.  [Distributed Computing](#distributed-computing)
6.  [Errors, Interrupts and Timeouts](#errors-interrupts-and-timeouts)
7.  [Deferred Evaluation Pipe](#deferred-evaluation-pipe)
8.  [Links](#links)

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
#>  num [1:100000000] 0.49 0.613 -1.05 -1.045 4.273 ...
```

Alternatively, explicitly call and wait for the result using
`call_mirai()`.

``` r
call_mirai(m)$data |> str()
#>  num [1:100000000] 0.49 0.613 -1.05 -1.045 4.273 ...
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

### Daemons

Daemons or persistent background processes may be set to receive ‘mirai’
requests.

This is potentially more efficient as new processes no longer need to be
created on an *ad hoc* basis.

##### Passive Queue (default)

Call daemons() with the number of daemons to launch.

``` r
daemons(8)
#> [1] 8
```

Call `daemons_view()` to view the number of daemons, and also the number
of active connections. In the default implementation, the background
processes connect directly into the client and the number of daemons and
connections will be the same.

``` r
daemons_view()
#> $daemons
#> [1] 8
#> 
#> $connections
#> [1] 8
```

This low-level implementation ensures tasks are evenly-distributed
amongst daemons, and provides a robust and resource-light solution. This
is particularly suited to working with similar-length tasks, or where
the number of concurrent tasks typically does not exceed the number of
available daemons.

``` r
daemons(0)
#> [1] 0
```

Set the number of daemons to zero to reset. This reverts to the default
of creating a new background process for each ‘mirai’ request.

##### Active Queue

Specifying the argument `q = TRUE` provides access to an alternative
approach, which implements an active queue (task scheduler).

``` r
daemons(8, q = TRUE)
#> [1] 8
```

Calling `daemons_view()` shows 8 daemons, but only one connection. This
is as the queue now acts as a bridge between the client and individual
daemon processes.

``` r
daemons_view()
#> $daemons
#> [1] 8
#> 
#> $connections
#> [1] 1
```

The queue consumes additional resources, however ensures optimal
allocation of tasks to daemons such that they are run as soon as
resources become available.

``` r
daemons(0)
#> [1] 0
```

Set the number of daemons to zero to reset.

[« Back to ToC](#table-of-contents)

### Distributed Computing

The `daemons()` interface may also be used to send tasks for computation
to server processes on the network.

Instead of specifying a numeric value, instead specify as a character
string the client network address and a port that is able to accept
incoming connections.

For example if your local network address is ‘192.168.0.2’, make sure
that a port e.g. ‘5555’ is available for inbound connections from the
local network.

Alternatively, simply supply a colon followed by the port number to
listen on all interfaces on the host, for example:

``` r
# daemons("tcp://192.168.0.2:5555")
daemons("tcp://:5555")
#> [1] 1
```

The network topology is such that the client listens at the above
address, and distributes tasks to all connected server processes.

On the server, `server()` may be called from an R session, or an Rscript
invocation. This sets up a remote daemon process that connects to the
client network IP address and receives tasks:

    Rscript --vanilla -e 'mirai::server("tcp://192.168.0.2:5555")'

Alternatively, use `serverq()` to launch a queue directing a cluster of
\[8\] daemons:

    Rscript --vanilla -e 'mirai::serverq(8,"tcp://192.168.0.2:5555")'

Calling `daemons_view()` will now always show one daemon. However
network resources may be added and removed as required, and tasks are
automatically distributed to all available server processes. The number
of connections will show the actual number of instances connected into
the client (2 in the example below).

``` r
daemons_view()
#> $daemons
#> [1] 1
#> 
#> $connections
#> [1] 1
```

To reset all connections and revert to default behaviour:

``` r
daemons(0)
#> [1] 0
```

This also sends an exit signal to connected server instances so that
they exit automatically.

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

m2 <- mirai(mirai())
call_mirai(m2)$data
#> 'miraiError' chr Error in mirai(): missing expression, perhaps wrap in {}?

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
m <- mirai({Sys.sleep(0.5); 1})
b <- m$data %>>% c(2, 3) %>>% as.character()
b
#> < unresolvedExpr >
#>  - $data to query resolution
b$data
#> < unresolvedExpr >
#>  - $data to query resolution
Sys.sleep(1)
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
