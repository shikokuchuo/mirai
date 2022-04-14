# mirai 0.4.0.9000

* Now exits cleanly when daemons have not been explicitly zeroed prior to ending an R session.

# mirai 0.4.0

* Back to a pure R implementation thanks to enhanced internal design at nanonext.
* Adds auxiliary function `is_mirai()` to test if an object is a mirai.
* Versioning system to synchronise with nanonext e.g. v0.4.x requires nanonext >= 0.4.0.

# mirai 0.2.0

* The value of a mirai is now stored at `$data` to optimally align with the underlying implementation.
* Package now contains C code (requires compilation), using weak references for simpler management of resources.
* Switch to abstract sockets on Linux.

# mirai 0.1.1

* `mirai()` added as an alias for `eval_mirai()`; supports evaluating arbitrary length expressions wrapped in `{}`.
* A mirai now resolves automatically without requiring `call_mirai()`. Access the `$value` directly and an ‘unresolved’ logical NA will be returned if the async operation is yet to complete.
* `stop_mirai()` added as a function to stop evaluation of an ongoing async operation.
* Auxiliary functions `is_nul_byte()` and `unresolved()` re-exported from {nanonext} to test for evaluation errors and resolution of a 'mirai' respectively.
* New `daemons()` interface to set and manage persistent background processes for receiving 'mirai' requests.

# mirai 0.1.0

* Initial release.
