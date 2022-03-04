# mirai 0.1.0.9005 (development)

* `eval_mirai()` adds support for evaluating arbitrary length expressions wrapped in `{}`.
* The evaluated result of a mirai is now stored at `$data` and may be accessed at any time, returning an 'unresolved' logical NA if the async operation is yet to complete. Use `call_mirai()` to call and wait for  completion of an Aio.
* Auxiliary functions `is_nul_byte()` and `unresolved()` re-exported from the nanonext package.
* New `mirai()` daemon manager for persistent background processes [experimental].

# mirai 0.1.0

* Initial release.
