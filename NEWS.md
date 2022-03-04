# mirai 0.1.0.9005 (development)

* `eval_mirai()` adds support for evaluating arbitrary length expressions wrapped in `{}`.
* The evaluated result of a 'mirai' is now stored at `$data` and may be accessed at any time, returning an 'unresolved' logical NA value if the async operation is yet to complete. Use `call_mirai()` to call and wait for completion of a 'mirai'.
* `stop_mirai()` added as a function to stop evaluation of an ongoing async operation.
* Auxiliary functions `is_nul_byte()` and `unresolved()` re-exported from {nanonext} to test for evaluation errors and resolution of a 'mirai' respectively.
* New `mirai()` control panel to manage package-wide settings such as persistent background processes (daemons) [experimental].

# mirai 0.1.0

* Initial release.
