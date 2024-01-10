
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
#>  [1]  -1.43344968  -0.02044095  -0.54523216   3.66578804   5.10911257
#>  [6]   0.19572871   0.27279264  -1.83408111 -48.92140310  -0.69761779
```

Alternatively, explicitly call and wait for the result using
`call_mirai()`.

``` r
call_mirai(m)$data
#>  [1]  -1.43344968  -0.02044095  -0.54523216   3.66578804   5.10911257
#>  [6]   0.19572871   0.27279264  -1.83408111 -48.92140310  -0.69761779
```

### Daemons

Daemons are persistent background processes created to receive ‘mirai’
requests. This is potentially more efficient as new processes no longer
need to be created on an *ad hoc* basis.

They may be deployed
[locally](https://shikokuchuo.net/mirai/articles/mirai.html#daemons-local-persistent-processes)
or
[remotely](https://shikokuchuo.net/mirai/articles/mirai.html#distributed-computing-remote-daemons),
even
[launched](https://shikokuchuo.net/mirai/articles/mirai.html#distributed-computing-launching-daemons)
across the network via SSH etc., optionally using
automatically-configured [secure TLS
connections](https://shikokuchuo.net/mirai/articles/mirai.html#distributed-computing-tls-secure-connections).

Refer to the [mirai
vignette](https://shikokuchuo.net/mirai/articles/mirai.html) for full
package functionality. This may be accessed within R by:

``` r
vignette("mirai", package = "mirai")
```

### Powering Crew and Targets

The [`crew`](https://cran.r-project.org/package=crew) package is a
distributed worker-launcher extending {mirai} to different distributed
computing platforms, from traditional clusters to cloud services.

[`crew.cluster`](https://cran.r-project.org/package=crew.cluster)
enables mirai-based workflows on traditional high-performance computing
clusters using:

- LFS
- PBS/TORQUE
- SGE
- SLURM

[`crew.aws.batch`](https://cran.r-project.org/package=crew.aws.batch)
extends {mirai} to cloud computing using AWS Batch.

[`targets`](https://cran.r-project.org/package=targets), a Make-like
pipeline tool for statistics and data science, has integrated and
adopted [`crew`](https://cran.r-project.org/package=crew) as its default
high-performance computing backend.

### Parallel Clusters

{mirai} provides an alternative communications backend for R’s
‘parallel’ base package, implementing a low-level feature request by
R-Core at [R Project Sprint
2023](https://contributor.r-project.org/r-project-sprint-2023/).

``` r
cl <- make_cluster(4)
cl
#> < miraiCluster | ID: `0` nodes: 4 active: TRUE >
```

A ‘miraiCluster’ is fully compatible with all ‘parallel’ functions such
as `parallel::clusterApply()` \[[further
details](https://shikokuchuo.net/mirai/articles/parallel.html)\].

### Asynchronous Shiny and Plumber Applications

{mirai} serves as an asynchronous backend for scaling enterprise {shiny}
or {plumber} applications.

A ‘mirai’ plugs in directly to Shiny’s reactive framework without the
need to use promises \[[see
example](https://shikokuchuo.net/mirai/articles/shiny.html#shiny-example-usage)\].

Alternatively, ‘mirai’ may be used interchangeably with ‘promises’ by
using the promise pipe `%...>%`, or explictly by
`promises::as.promise()`, allowing side-effects to be performed upon
asynchronous resolution of a ‘mirai’.

The following example outputs “hello” to the console after one second
when the ‘mirai’ resolves.

``` r
library(promises)
p <- mirai({Sys.sleep(1); "hello"}) %...>% cat()
p
#> <Promise [pending]>
```

Example usage is provided for
[shiny](https://shikokuchuo.net/mirai/articles/shiny.html) and for
[plumber](https://shikokuchuo.net/mirai/articles/plumber.html).

### Torch Parallelization

The custom serialization interface in {mirai} is accessed via
`serialization()`.

In the case of {torch}, this requires just the following call at the
head of your session:

``` r
serialization(refhook = list(torch::torch_serialize, torch::torch_load))
```

This allows tensors, including complex objects such as models,
optimizers etc. to be used seamlessly across local and remote processes
in the same way as other R objects \[[further
details](https://shikokuchuo.net/mirai/articles/torch.html)\].

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

[Luke Tierney](https://github.com/ltierney/), R Core, for discussion on
R’s implementation of L’Ecuyer-CMRG streams, used to ensure statistical
independence in parallel processing.

[Daniel Falbel](https://github.com/dfalbel/), for discussion around an
efficient solution to serialization and transmission of
[`torch`](https://cran.r-project.org/package=torch) tensors.

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
