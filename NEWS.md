# mirai 0.1.0.9003 (development)

* The evaluated result of a mirai is now stored at `$data` rather than `$value`. The result may be queried at any time and will return an NA 'unresolved value' if the async operation is yet to complete.
* `eval_mirai()` adds support for evaluating arbitrary length expressions wrapped in `{}`.
* Auxiliary functions `is_nul_byte()` and `is_resolved()` re-exported from the nanonext package.
* New `mirai()` daemon manager for persistent background processes [experimental].

# mirai 0.1.0

* Initial release.
