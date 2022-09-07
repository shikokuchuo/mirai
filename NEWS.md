# mirai 0.5.3.9001 (development)

* Internal enhancements to `daemons()` interface requiring nanonext >= 0.5.5.

# mirai 0.5.3

* `mirai()` gains a '.args' argument for passing a list of objects already in the calling environment, allowing for example `mirai(func(x, y, z), .args = list(x, y, z))` rather than having to specify `mirai(func(x, y, z), x = x, y = y, z = z)`.
* Errors from inside a mirai will now return the error message as a character string of class 'miraiError' and 'errorValue', rather than just a nul byte. Utility function `is_mirai_error()` should be used in place of `is_nul_byte()`, which is no longer re-exported.
* `is_error_value()` can be used to test for all errors, including timeouts where the '.timeout' argument has been used.
* All re-exports from 'nanonext' have been brought in-package for better documentation.

# mirai 0.5.2

* Internal optimisations requiring nanonext >= 0.5.2.

# mirai 0.5.0

* Implements the `%>>%` deferred evaluation pipe.
* Adds '.timeout' argument to `mirai()` to ensure a mirai always resolves even if the child process crashes etc.

# mirai 0.4.1

* Exits cleanly when daemons have not been explicitly zeroed prior to ending an R session.
* Fixes possible hang on Windows when shutting down daemons.

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
