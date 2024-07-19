# hicp 0.6.0

* Introduced the global options `options("hicp.unbundle"=TRUE)`, `options(hicp.coicop.version="ecoicop-hicp")`, `options(hicp.all.items.code="00")`
* The functions `is.coicop()`, `level()`, `parent()`, `child()`, `aggregate()`, and `tree()` now have the argument `settings`, which can be used to define a COICOP version (`coicop.version`), the all-items code (`all.items.code`), and how bundle codes are treated (`unbundle`)
* The function `is.coicop()` now accepts different COICOP versions. The functions `level()`, `parent()`, `child()`, `aggregate()`, and `tree()` only process valid COICOP codes and the all-items code.
* Fixed the behavior of `aggregate()` when the functions `jevons()`, `carli()`, and `harmonic()` are used and no weights `w0` are provided
* Fixed a bug in the function `chain()`, which occurred when the unchained series started in January and `by=12`. In this case, the index reference period (with value 100) was not correctly detected
* Updated documentation and tests accordingly

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
