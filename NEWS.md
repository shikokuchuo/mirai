# mirai 0.1.0.9007 (development)

* `mirai()` added as an alias for `eval_mirai()`; supports evaluating arbitrary length expressions wrapped in `{}`.
* The evaluated result of a 'mirai' is now stored at `$data` and may be accessed at any time, returning an 'unresolved' logical NA if the async operation is yet to complete. Use `call_mirai()` to call and wait for completion of a 'mirai'.
* `stop_mirai()` added as a function to stop evaluation of an ongoing async operation.
* Auxiliary functions `is_nul_byte()` and `unresolved()` re-exported from {nanonext} to test for evaluation errors and resolution of a 'mirai' respectively.
* New `daemons()` interface to set and manage persistent background processes for receiving 'mirai' requests.

# mirai 0.1.0

* Initial release.
