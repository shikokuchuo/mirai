
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mirai <a href="https://shikokuchuo.net/mirai/" alt="mirai"><img src="man/figures/logo.png" alt="mirai logo" align="right" width="120"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/mirai?color=112d4e)](https://CRAN.R-project.org/package=mirai)
[![mirai status
badge](https://shikokuchuo.r-universe.dev/badges/mirai?color=24a60e)](https://shikokuchuo.r-universe.dev/mirai)
[![R-CMD-check](https://github.com/shikokuchuo/mirai/workflows/R-CMD-check/badge.svg)](https://github.com/shikokuchuo/mirai/actions)
[![codecov](https://codecov.io/gh/shikokuchuo/mirai/graph/badge.svg)](https://app.codecov.io/gh/shikokuchuo/mirai)
[![DOI](https://zenodo.org/badge/459341940.svg)](https://zenodo.org/badge/latestdoi/459341940)
<!-- badges: end -->

### みらい

<br /> ( futuristic ・ whisper ) <br /><br /> Minimalist Async
Evaluation Framework for R <br /><br /> Lightweight parallel code
execution and distributed computing. <br /><br /> `mirai()` returns a
‘mirai’ object immediately. Designed for simplicity, a ‘mirai’ evaluates
an R expression asynchronously, on local or network resources, resolving
automatically upon completion. <br /><br /> State of the art networking
and concurrency via [nanonext](https://doi.org/10.5281/zenodo.7903429)
offers reliable and efficient scheduling over fast inter-process
communications or TCP/IP secured by TLS.

<div align="right">

*mirai パッケージを試してみたところ、かなり速くて驚きました*

</div>

### Installation

Releases are made on rOpenSci R-universe, and also CRAN:

``` r
install.packages("mirai", repos = c("shikokuchuo.r-universe.dev", "cloud.r-project.org"))
```

Install the latest development version from Github:

``` r
remotes::install_github("shikokuchuo/mirai")
```

### Quick Start

Use `mirai()` to evaluate an expression asynchronously in a separate,
clean R process.

A ‘mirai’ object is returned immediately.

``` r
library(mirai)

m <- mirai(
  {
    res <- rnorm(x) + y ^ 2
    res / rev(res)
  },
  x = 10,
  y = runif(1)
)

m
#> < mirai | $data >
```

Above, all specified `name = value` pairs are passed through to the
‘mirai’.

The ‘mirai’ yields an ‘unresolved’ logical NA whilst the async operation
is ongoing.

``` r
m$data
#> 'unresolved' logi NA
```

To check whether a mirai has resolved:

``` r
unresolved(m)
#> [1] FALSE
```

Upon completion, the ‘mirai’ resolves automatically to the evaluated
result.

``` r
m$data
#>  [1] 0.6241584 1.8421814 6.1217990 1.0420236 0.7611066 1.3138764 0.9596711
#>  [8] 0.1633507 0.5428347 1.6021574
```

Alternatively, explicitly call and wait for the result using
`call_mirai()`.

``` r
call_mirai(m)$data
#>  [1] 0.6241584 1.8421814 6.1217990 1.0420236 0.7611066 1.3138764 0.9596711
#>  [8] 0.1633507 0.5428347 1.6021574
```

### Daemons

Daemons are persistent background processes created to receive ‘mirai’
requests.

They may be deployed for:

[Local](https://shikokuchuo.net/mirai/articles/mirai.html#daemons-local-persistent-processes)
parallel processing; or

[Remote](https://shikokuchuo.net/mirai/articles/mirai.html#distributed-computing-remote-daemons)
network distributed computing.

[Launchers](https://shikokuchuo.net/mirai/articles/mirai.html#distributed-computing-launching-daemons)
allow daemons to be started both on the local machine and across the
network via SSH etc.

[Secure TLS
connections](https://shikokuchuo.net/mirai/articles/mirai.html#distributed-computing-tls-secure-connections)
can be automatically-configured on-the-fly for remote daemon
connections.

Refer to the [{mirai}
vignette](https://shikokuchuo.net/mirai/articles/mirai.html) for full
package functionality. This may be accessed within R by:

``` r
vignette("mirai", package = "mirai")
```

### Integrations

The following core integrations are documented, with usage examples in
the linked vignettes:

[{parallel}](https://shikokuchuo.net/mirai/articles/parallel.html) -
provides an alternative communications backend for R, implementing a
low-level feature request by R-Core at R Project Sprint 2023.

[{promises}](https://shikokuchuo.net/mirai/articles/promises.html) -
‘mirai’ may be used interchangeably with ‘promises’ by using the promise
pipe `%...>%` or the `as.promise()` method.

[{plumber}](https://shikokuchuo.net/mirai/articles/plumber.html) -
serves as an asynchronous / distributed backend, scaling applications
via the use of promises.

[{shiny}](https://shikokuchuo.net/mirai/articles/shiny.html) - serves as
an asynchronous / distributed backend, plugging directly into the
reactive framework without the need for promises.

[{torch}](https://shikokuchuo.net/mirai/articles/torch.html) - the
custom serialization interface allows tensors and complex objects such
as models and optimizers to be used seamlessly across parallel
processes.

### Powering Crew and Targets High Performance Computing

[{targets}](https://docs.ropensci.org/targets/), a Make-like pipeline
tool for statistics and data science, has integrated and adopted {crew}
as its default high-performance computing backend.

[{crew}](https://wlandau.github.io/crew/) is a distributed
worker-launcher extending {mirai} to different distributed computing
platforms, from traditional clusters to cloud services.

[{crew.cluster}](https://wlandau.github.io/crew.cluster/) enables
mirai-based workflows on traditional high-performance computing clusters
using LFS, PBS/TORQUE, SGE and SLURM.

[{crew.aws.batch}](https://wlandau.github.io/crew.aws.batch/) extends
{mirai} to cloud computing using AWS Batch.

### Thanks

We would like to thank in particular:

[Will Landau](https://github.com/wlandau/), for being instrumental in
shaping development of the package, from initiating the original request
for persistent daemons, through to orchestrating robustness testing for
the high performance computing requirements of {crew} and {targets}.

[Henrik Bengtsson](https://github.com/HenrikBengtsson/), for valuable
and incisive insights leading to the interface accepting broader usage
patterns.

[Luke Tierney](https://github.com/ltierney/), R Core, for discussion on
R’s implementation of L’Ecuyer-CMRG streams, used to ensure statistical
independence in parallel processing.

[Daniel Falbel](https://github.com/dfalbel/), for discussion around an
efficient solution to serialization and transmission of {torch} tensors.

[« Back to ToC](#table-of-contents)

### Links

mirai website: <https://shikokuchuo.net/mirai/><br /> mirai on CRAN:
<https://cran.r-project.org/package=mirai>

Listed in CRAN Task View: <br /> - High Performance Computing:
<https://cran.r-project.org/view=HighPerformanceComputing>

nanonext website: <https://shikokuchuo.net/nanonext/><br /> nanonext on
CRAN: <https://cran.r-project.org/package=nanonext>

NNG website: <https://nng.nanomsg.org/><br />

[« Back to ToC](#table-of-contents)

–

Please note that this project is released with a [Contributor Code of
Conduct](https://shikokuchuo.net/mirai/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
