# mirai 0.1.1.9001 (development)

* The value of a mirai is now stored at `$data` to optimally align with the underlying implementation.
* Package now contains C code and requires compilation when installing from source.
* Switch to abstract sockets on Linux.

# mirai 0.1.1

* `mirai()` added as an alias for `eval_mirai()`; supports evaluating arbitrary length expressions wrapped in `{}`.
* A mirai now resolves automatically without requiring `call_mirai()`. Access the `$value` directly and an ‘unresolved’ logical NA will be returned if the async operation is yet to complete.
* `stop_mirai()` added as a function to stop evaluation of an ongoing async operation.
* Auxiliary functions `is_nul_byte()` and `unresolved()` re-exported from {nanonext} to test for evaluation errors and resolution of a 'mirai' respectively.
* New `daemons()` interface to set and manage persistent background processes for receiving 'mirai' requests.

# mirai 0.1.0

* Initial release.
