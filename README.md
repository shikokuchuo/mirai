
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mirai <a href="https://shikokuchuo.net/mirai/" alt="mirai"><img src="man/figures/logo.png" alt="mirai logo" align="right" width="120"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/mirai?color=112d4e)](https://CRAN.R-project.org/package=mirai)
[![mirai status
badge](https://shikokuchuo.r-universe.dev/badges/mirai?color=24a60e)](https://shikokuchuo.r-universe.dev/mirai)
[![R-CMD-check](https://github.com/shikokuchuo/mirai/workflows/R-CMD-check/badge.svg)](https://github.com/shikokuchuo/mirai/actions)
[![codecov](https://codecov.io/gh/shikokuchuo/mirai/branch/main/graph/badge.svg)](https://app.codecov.io/gh/shikokuchuo/mirai)
[![DOI](https://zenodo.org/badge/459341940.svg)](https://zenodo.org/badge/latestdoi/459341940)
<!-- badges: end -->

Minimalist async evaluation framework for R. <br /><br /> Lightweight
parallel code execution and distributed computing. <br /><br /> Designed
for simplicity, a ‘mirai’ evaluates an R expression asynchronously, on
local or network resources, resolving automatically upon completion.
<br /><br /> `mirai()` returns a ‘mirai’ object immediately. ‘mirai’
(未来 みらい) is Japanese for ‘future’. <br /><br /> Efficient
scheduling over fast inter-process communications or secure TLS
connections over TCP/IP, built on ‘nanonext’ and ‘NNG’ (Nanomsg Next
Gen). <br /><br /> {mirai} has a tiny pure R code base, relying solely
on [`nanonext`](https://doi.org/10.5281/zenodo.7903429), a
high-performance binding for the ‘NNG’ (Nanomsg Next Gen) C library with
zero package dependencies. <br /><br />

### Installation

Install the latest release from CRAN:

``` r
install.packages("mirai")
```

or the development version from rOpenSci R-universe:

``` r
install.packages("mirai", repos = "https://shikokuchuo.r-universe.dev")
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
  x = 11,
  y = runif(1)
)

m
#> < mirai >
#>  - $data for evaluated result
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
#>  [1]  -0.04026068  -1.92115491   0.17933997   0.69404292   0.01749486
#>  [6]   1.00000000  57.15965086   1.44083309   5.57600189  -0.52052023
#> [11] -24.83812992
```

Alternatively, explicitly call and wait for the result using
`call_mirai()`.

``` r
call_mirai(m)$data
#>  [1]  -0.04026068  -1.92115491   0.17933997   0.69404292   0.01749486
#>  [6]   1.00000000  57.15965086   1.44083309   5.57600189  -0.52052023
#> [11] -24.83812992
```

### Vignette

See the [mirai
vignette](https://shikokuchuo.net/mirai/articles/mirai.html) for full
package functionality.

Key topics include:

- Example use cases

- Local daemons - persistent background processes

- Distributed computing - remote daemons

- Secure TLS connections

- Serialization - registering custom functions

This may be accessed within R by:

``` r
vignette("mirai", package = "mirai")
```

### Use with Parallel and Foreach

{mirai} provides an alternative communications backend for R’s base
‘parallel’ package.

``` r
cl <- make_cluster(4)
cl
#> < miraiCluster >
#>  - cluster ID: `0`
#>  - nodes: 4
#>  - active: TRUE
```

`make_cluster()` creates a ‘miraiCluster’, a cluster fully compatible
with all ‘parallel’ functions such as:

- `parallel::clusterApply()`
- `parallel::parLapply()`
- `parallel::parLapplyLB()`

A ‘miraiCluster’ may also be registered for use with the
[`foreach`](https://cran.r-project.org/package=foreach) package by
[`doParallel`](https://cran.r-project.org/package=doParallel).

This functionality fulfils a request from R-Core at R Project Sprint
2023.

### Use with Crew and Targets

The [`crew`](https://cran.r-project.org/package=crew) package is a
distributed worker-launcher extending {mirai} to different distributed
computing platforms, from traditional clusters to cloud services.

[`crew.cluster`](https://cran.r-project.org/package=crew.cluster) is a
plug-in that enables mirai-based workflows on traditional
high-performance computing clusters using:

- LFS
- PBS/TORQUE
- SGE
- SLURM

[`targets`](https://cran.r-project.org/package=targets), a Make-like
pipeline tool for statistics and data science, has integrated and
adopted [`crew`](https://cran.r-project.org/package=crew) as its default
recommended high-performance computing backend.

### Use with Shiny and Plumber

{mirai} serves as a backend for enterprise asynchronous
[`shiny`](https://cran.r-project.org/package=shiny) or
[`plumber`](https://cran.r-project.org/package=plumber) applications.

A ‘mirai’ may be used interchangeably with a ‘promise’ by using the the
promise pipe `%...>%`, or explictly by `promises::as.promise()`,
allowing side-effects to be performed upon asynchronous resolution of a
‘mirai’.

The following example outputs “hello” to the console after one second
when the ‘mirai’ resolves.

``` r
library(promises)

p <- mirai({Sys.sleep(1); "hello"}) %...>% cat()
p
#> <Promise [pending]>
```

Alternatively, [`crew`](https://cran.r-project.org/package=crew)
provides an interface that facilitates deploying {mirai} for
[`shiny`](https://cran.r-project.org/package=shiny).

- Please refer to its [Asynchronous Shiny
  Apps](https://wlandau.github.io/crew/articles/shiny.html) vignette,
  which features a tutorial and sample code.

### Use with Torch

The custom serialization interface in {mirai} is accessed via the
`serialization()` function.

In the case of [`torch`](https://cran.r-project.org/package=torch), this
would involve making the following call once at the start of your
session:

``` r
serialization(refhook = list(torch:::torch_serialize, torch::torch_load))
#> [ mirai ] serialization functions registered
```

- Note that `torch_serialize()` is available via `:::` since
  [`torch`](https://cran.r-project.org/package=torch) v0.9.0, and will
  be exported in v0.12.0.

This allows tensors, including models, optimizers etc. to be used
seamlessly across local and remote processes like any other R object.

For more details, please refer to the relevant [vignette
chapter](https://shikokuchuo.net/mirai/articles/mirai.html#serialization-custom-functions).

### Thanks

We would like to thank in particular:

[William Landau](https://github.com/wlandau/), for being instrumental in
shaping development of the package, from initiating the original request
for persistent daemons, through to orchestrating robustness testing for
the high performance computing requirements of
[`crew`](https://cran.r-project.org/package=crew) and
[`targets`](https://cran.r-project.org/package=targets).

[Henrik Bengtsson](https://github.com/HenrikBengtsson/), for valuable
and incisive insights leading to the interface accepting broader usage
patterns.

[Luke Tierney](https://github.com/ltierney/), R Core, for introducing
R’s implementation of L’Ecuyer-CMRG streams, used to ensure statistical
independence in parallel processing.

[Daniel Falbel](https://github.com/dfalbel/), for discussion around an
efficient solution to serialization and transmission of ‘torch’ tensors.

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
