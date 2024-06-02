# hicp 0.5.1

* Fixed a bug in the functions `jevons()`, `carli()`, and `harmonic()`, which checked the weights `w0` that were, however, not required by the function
* Fixed a bug in the function `chain()`, which occurred when `NA`'s were present in the index where the index value was set to 100 (instead of `NA`)
* Updated documentation and tests accordingly

# hicp 0.5.0

* New functions `jevons()`, `carli()`, and `harmonic()` available
* Function `aggregate()` now allows calculation of user-defined aggregates even if not all parts of an aggregate are present in the data (`settings$add.exact=FALSE`)
* Function `aggregate()` no longer requires any weights `w0` or `wt` if the underlying `index` function does not require weights

# hicp 0.4.2

* Function `link()` now returns a matrix instead of a data.table
* Function `convert()` now returns a named vector instead of a data.table
* Updated references to the new HICP Methodological Manual
* Cleaned code for easier reference to other package functions

# hicp 0.4.1

* Updated description
* Replaced `\dontrun` with `\donttest` in examples

# hicp 0.4.0

* Initial CRAN submission.
* First Github release.
