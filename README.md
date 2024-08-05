
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mirai <a href="https://shikokuchuo.net/mirai/" alt="mirai"><img src="man/figures/logo.png" alt="mirai logo" align="right" width="120"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/mirai?color=00184a)](https://CRAN.R-project.org/package=mirai)
[![R-multiverse
status](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fcommunity.r-multiverse.org%2Fapi%2Fpackages%2Fmirai&query=%24.Version&label=r-multiverse&color=00184a)](https://community.r-multiverse.org/mirai)
[![R-universe
status](https://shikokuchuo.r-universe.dev/badges/mirai?color=2dab18)](https://shikokuchuo.r-universe.dev/mirai)
[![R-CMD-check](https://github.com/shikokuchuo/mirai/workflows/R-CMD-check/badge.svg)](https://github.com/shikokuchuo/mirai/actions)
[![codecov](https://codecov.io/gh/shikokuchuo/mirai/graph/badge.svg)](https://app.codecov.io/gh/shikokuchuo/mirai)
[![DOI](https://zenodo.org/badge/459341940.svg)](https://zenodo.org/badge/latestdoi/459341940)
<!-- badges: end -->

### ミライ

<br /> ( 未来 ) <br /><br /> Minimalist Async Evaluation Framework for R
<br /><br /> Designed for simplicity, a ‘mirai’ evaluates an R
expression asynchronously in a parallel process, locally or distributed
over the network, with the result automatically available upon
completion. <br /><br /> Modern networking and concurrency built on
[nanonext](https://github.com/shikokuchuo/nanonext/) and
[NNG](https://nng.nanomsg.org/) (Nanomsg Next Gen) ensures reliable and
efficient scheduling, over fast inter-process communications or TCP/IP
secured by TLS. <br /><br /> Advantages include being inherently queued
thus handling many more tasks than available processes, no storage on
the file system, support for otherwise non-exportable reference objects,
an event-driven promises implementation, and built-in asynchronous
parallel map. <br /><br />

### Quick Start

Use `mirai()` to evaluate an expression asynchronously in a separate,
clean R process.

The following mimics an expensive calculation that eventually returns a
random value.

``` r
library(mirai)

x <- list(time = 2, mean = 4)

m <- mirai({Sys.sleep(time); rnorm(1, mean)}, time = x$time, mean = x$mean)
```

The mirai expression is evaluated in another process and hence must be
self-contained, not referring to variables that do not already exist
there. Above, the variables `time` and `mean` are passed as part of the
`mirai()` call.

A ‘mirai’ object is returned immediately - creating a mirai never blocks
the session.

Whilst the async operation is ongoing, attempting to access a mirai’s
data yields an ‘unresolved’ logical NA.

``` r
m
#> < mirai [] >
m$data
#> 'unresolved' logi NA
```

To check whether a mirai remains unresolved i.e. its async operation is
still ongoing:

``` r
unresolved(m)
#> [1] TRUE
```

To wait for and collect the return value, use the mirai’s `[]` method:

``` r
m[]
#> [1] 4.370811
```

As a mirai represents an async operation, it is never necessary to wait
for it. Other code can continue to be run. Once it completes, the return
value automatically becomes available at `$data`.

``` r
while (unresolved(m)) {
  # do work here that does not depend on 'm'
}
m
#> < mirai [$data] >
m$data
#> [1] 4.370811
```

#### Daemons

Daemons are persistent background processes for receiving mirai
requests, and are created as easily as:

``` r
daemons(4)
#> [1] 4
```

Daemons may also be deployed
[remotely](https://shikokuchuo.net/mirai/articles/mirai.html#distributed-computing-remote-daemons)
for distributed computing and
[launchers](https://shikokuchuo.net/mirai/articles/mirai.html#distributed-computing-launching-daemons)
can start daemons across the network via (tunnelled) SSH or a cluster
resource manager.

[Secure TLS
connections](https://shikokuchuo.net/mirai/articles/mirai.html#distributed-computing-tls-secure-connections)
can be used for remote daemon connections, with zero configuration
required.

#### Async Parallel Map

`mirai_map()` maps a function over a list or vector, with each element
processed in a separate parallel process. It also performs multiple map
over 2D lists/vectors, allowing advanced patterns such as map over the
rows of a dataframe or matrix.

``` r
df <- data.frame(
  fruit = c("melon", "pear", "coconut"),
  price = c(5L, 1L, 2L)
)
m <- mirai_map(
  df,
  function(fruit, currency, price)
    sprintf("The price of a %s is %s%d.", fruit, currency, price),
  .args = list(currency = "$")
)
```

A ‘mirai_map’ object is returned immediately. Other code can continue to
run at this point. Its value may be retrieved at any time using its `[]`
method to return a list, just like `purrr::map()` or `base::lapply()`.
The `[]` method also provides options for flatmap, early stopping and/or
progress indicators.

``` r
m
#> < mirai map [3/3] >
m[.flat]
#> [1] "The price of a melon is $5."   "The price of a pear is $1."   
#> [3] "The price of a coconut is $2."
```

All errors are returned as ‘errorValues’, facilitating recovery from
partial failure. There are further
[advantages](https://shikokuchuo.net/mirai/articles/mirai.html#asynchronous-parallel-map)
over alternative map implementations.

### Design Concepts

`mirai` is designed from the ground up to provide a production-grade
experience.

- Fast
  - Over 100x more responsive than common alternatives <sup>\[1\]</sup>
  - Built for low-latency applications such as real time inference and
    Shiny apps
- Reliable
  - Consistent behaviour with no reliance on global options or variables
  - Each mirai call is evaluated explicitly for transparent and
    predictable results
- Scalable
  - Launch millions of tasks simultaneously over thousands of
    connections
  - Proven track record handling heavy-duty workloads in the life
    sciences industry

[<img alt="Joe Cheng on mirai with Shiny" src="https://img.youtube.com/vi/GhX0PcEm3CY/hqdefault.jpg" width = "300" height="225" />](https://youtu.be/GhX0PcEm3CY?t=1740)
 
[<img alt="Will Landau on mirai in clinical trials" src="https://img.youtube.com/vi/cyF2dzloVLo/hqdefault.jpg" width = "300" height="225" />](https://youtu.be/cyF2dzloVLo?t=5127)

> *mirai パッケージを試してみたところ、かなり速くて驚きました*

### Integrations

The following core integrations are documented, with usage examples in
the linked vignettes:

[<img alt="R parallel" src="https://www.r-project.org/logo/Rlogo.png" width="40" height="31" />](https://shikokuchuo.net/mirai/articles/parallel.html)
  Provides an alternative communications backend for R, implementing a
new parallel cluster type, a feature request by R-Core at R Project
Sprint 2023. ‘miraiCluster’ may also be used with `foreach` via
`doParallel`.

[<img alt="promises" src="https://docs.posit.co/images/posit-ball.png" width="40" height="36" />](https://shikokuchuo.net/mirai/articles/promises.html)
  Implements the next generation of completely event-driven, non-polling
promises. ‘mirai’ may be used interchageably with ‘promises’, including
with the promise pipe `%...>%`.

[<img alt="Shiny" src="https://github.com/rstudio/shiny/raw/main/man/figures/logo.png" width="40" height="46" />](https://shikokuchuo.net/mirai/articles/shiny.html)
  Asynchronous parallel / distributed backend, supporting the next level
of responsiveness and scalability for Shiny. Launches ExtendedTasks, or
plugs directly into the reactive framework for advanced uses.

[<img alt="Plumber" src="https://rstudio.github.io/cheatsheets/html/images/logo-plumber.png" width="40" height="46" />](https://shikokuchuo.net/mirai/articles/plumber.html)
  Asynchronous parallel / distributed backend, capable of scaling
Plumber applications in production usage.

[<img alt="Arrow" src="https://arrow.apache.org/img/arrow-logo_hex_black-txt_white-bg.png" width="40" height="46" />](https://shikokuchuo.net/mirai/articles/databases.html)
  Allows queries using the Apache Arrow format to be handled seamlessly
over ADBC database connections hosted in background processes.

[<img alt="torch" src="https://torch.mlverse.org/css/images/hex/torch.png" width="40" height="46" />](https://shikokuchuo.net/mirai/articles/torch.html)
  Allows Torch tensors and complex objects such as models and optimizers
to be used seamlessly across parallel processes.

### Powering Crew and Targets High Performance Computing

[<img alt="targets" src="https://github.com/ropensci/targets/raw/main/man/figures/logo.png" width="40" height="46" />](https://docs.ropensci.org/targets/)
  Targets, a Make-like pipeline tool for statistics and data science,
has integrated and adopted `crew` as its default high-performance
computing backend.

[<img alt="crew" src="https://github.com/wlandau/crew/raw/main/man/figures/logo.png" width="40" height="46" />](https://wlandau.github.io/crew/)
  Crew is a distributed worker-launcher extending `mirai` to different
distributed computing platforms, from traditional clusters to cloud
services.

[<img alt="crew.cluster" src="https://github.com/wlandau/crew.cluster/raw/main/man/figures/logo.png" width="40" height="46" />](https://wlandau.github.io/crew.cluster/)
  `crew.cluster` enables mirai-based workflows on traditional
high-performance computing clusters using LFS, PBS/TORQUE, SGE and
Slurm.

[<img alt="crew.aws.batch" src="https://github.com/wlandau/crew.aws.batch/raw/main/man/figures/logo.png" width="40" height="46" />](https://wlandau.github.io/crew.aws.batch/)
  `crew.aws.batch` extends `mirai` to cloud computing using AWS Batch.

### Thanks

We would like to thank in particular:

[Will Landau](https://github.com/wlandau/) for being instrumental in
shaping development of the package, from initiating the original request
for persistent daemons, through to orchestrating robustness testing for
the high performance computing requirements of `crew` and `targets`.

[Joe Cheng](https://github.com/jcheng5/) for optimising the `promises`
method to work seamlessly within Shiny, and prototyping event-driven
promises, which is implemented across `nanonext` and `mirai`.

[Luke Tierney](https://github.com/ltierney/) of R Core, for discussion
on L’Ecuyer-CMRG streams to ensure statistical independence in parallel
processing, and making it possible for `mirai` to be the first
‘alternative communications backend for R’.

[Henrik Bengtsson](https://github.com/HenrikBengtsson/) for valuable
insights leading to the interface accepting broader usage patterns.

[Daniel Falbel](https://github.com/dfalbel/) for discussion around an
efficient solution to serialization and transmission of `torch` tensors.

[Kirill Müller](https://github.com/krlmlr/) for discussion on using
‘daemons’ to host Arrow database connections.

[<img alt="R Consortium" src="https://www.r-consortium.org/wp-content/uploads/sites/13/2016/09/RConsortium_Horizontal_Pantone.png" width="100" height="22" />](https://www.r-consortium.org/) 
for funding work on the TLS implementation in `nanonext`, used to
provide secure connections in `mirai`.

### Installation

Install the latest release from CRAN or R-multiverse:

``` r
install.packages("mirai")
```

The current development version is available from R-universe:

``` r
install.packages("mirai", repos = "https://shikokuchuo.r-universe.dev")
```

### Links & References

◈ mirai R package: <https://shikokuchuo.net/mirai/> <br /> ◈ nanonext R
package: <https://shikokuchuo.net/nanonext/>

mirai is listed in CRAN High Performance Computing Task View: <br />
<https://cran.r-project.org/view=HighPerformanceComputing>

\[1\] Benchmark available in appendix of:
<https://shikokuchuo.net/user2024-conference/>

–

Please note that this project is released with a [Contributor Code of
Conduct](https://shikokuchuo.net/mirai/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
