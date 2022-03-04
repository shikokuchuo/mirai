
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mirai <a href="https://shikokuchuo.net/mirai/" alt="mirai"><img src="man/figures/logo.png" alt="mirai logo" align="right" width="120"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/mirai?color=112d4e)](https://CRAN.R-project.org/package=mirai)
[![mirai status
badge](https://shikokuchuo.r-universe.dev/badges/mirai?color=ddcacc)](https://shikokuchuo.r-universe.dev)
[![R-CMD-check](https://github.com/shikokuchuo/mirai/workflows/R-CMD-check/badge.svg)](https://github.com/shikokuchuo/mirai/actions)
<!-- badges: end -->

Minimalist async evaluation framework for R.

未来 みらい mirai is Japanese for ‘future’.

Extremely simple and lightweight method for concurrent / parallel code
execution, built on ‘nanonext’ and ‘NNG’ (Nanomsg Next Gen) technology.

### Design Notes

Whilst frameworks for parallelisation exist for R, {mirai} is designed
for simplicity.

The package revolves around one single function:

-   `eval_mirai()` to send an expression for async evaluation, returning
    a ‘mirai’ which resolves automatically

Demonstrates the capability of {nanonext} in providing a lightweight and
robust cross-platform concurrency framework.

{mirai} has a tiny pure R code base, relying on a single package -
{nanonext}.

{nanonext} itself is a lightweight wrapper for the NNG C library with
zero package dependencies.

### Installation

Install the latest release from CRAN:

``` r
install.packages("mirai")
```

or the development version from rOpenSci R-universe:

``` r
options(repos = c(shikokuchuo = 'https://shikokuchuo.r-universe.dev', CRAN = 'https://cloud.r-project.org'))
install.packages("mirai")
```

### Demonstration

Use cases:

-   minimise execution times by performing long-running tasks
    concurrently in separate processes
-   ensure execution flow of the main process is not blocked

``` r
library(mirai)
```

#### Example 1: Compute-intensive Operations

Multiple long computes (model fits etc.) would take more time than if
performed concurrently on available computing cores.

Use `eval_mirai()` to evaluate an expression in a separate R process
asynchronously.

-   All named objects are passed through to a clean environment

A ‘mirai’ object is returned immediately.

``` r
mirai <- eval_mirai(rnorm(n) + m, n = 1e8, m = runif(1))
mirai
#> < mirai >
#>  - $data for evaluated result
mirai$data
#> < unresolved: logi NA >
```

The calculation is still ongoing hence an ‘unresolved’ logical NA value
is returned initially.

Continue running code concurrent to the async operation.

``` r
# do more...
```

When it completes, the ‘mirai’ will automatically return the evaluated
result when queried.

``` r
mirai$data |> str()
#> num [1:100000000] 0.7568 2.7849 1.2031 -0.0133 1.5456 ...
```

Alternatively, explicitly call and wait for the result (blocking) using
`call_mirai()`.

``` r
call_mirai(mirai)$data |> str()
#> num [1:100000000] 0.7568 2.7849 1.2031 -0.0133 1.5456 ...
```

#### Example 2: I/O-bound Operations

Processing high-frequency real-time data, writing results to
file/database can be slow and potentially disrupt the execution flow.

Cache data in memory and use `eval_mirai()` to perform periodic write
operations in a separate process.

A ‘mirai’ object is returned immediately.

``` r
mirai <- eval_mirai(write.csv(x, file = file), x = rnorm(1e8), file = tempfile())
```

Use `unresolved()` to poll for completion on a periodic basis, whilst
continuing to perform tasks.

``` r
while (unresolved(mirai)) {
  
  # do stuff
  
}
mirai$data
#> NULL
```

Peform other dependent tasks after confirming a successful write, such
as performing the next write etc.

### Links

{mirai} website: <https://shikokuchuo.net/mirai/><br /> {mirai} on CRAN:
<https://cran.r-project.org/package=mirai>

{nanonext} website: <https://shikokuchuo.net/nanonext/><br /> {nanonext}
on CRAN: <https://cran.r-project.org/package=nanonext>

NNG website: <https://nng.nanomsg.org/><br /> NNG documentation:
<https://nng.nanomsg.org/man/tip/><br />
