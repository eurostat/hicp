# hicp 1.0.0


**Breaking changes:**

1. Renamed the functions for downloading HICP data to `datasets()`, `datafilters()` and `data()`.

2. Added the new functions `aggregate.tree()` and `disaggregate()`.
Moved the functionality of `aggregate()` to `aggregate.tree()` to highlight that this function is used for the gradual index aggregation following the COICOP tree.
The functions `aggregate()` and `disaggregate()` can be used for the calculation of user-defined aggregates.

3. Renamed the argument `freq` in `convert()` to `type`, which is more in line with `rates()` and `contrib()`.

4. The function `rates()` now requires a date vector.
Before, the argument `t` was optional. 
This change makes the calculation faster, more robust and better aligned to other package functions.

5. Removed the list of COICOP bundles in `coicop.bundles`, which are now added to the global package options via `options("hicp.coicop.bundles")`. 
This allows users to adjust the list of COICOP bundles.

6. The function `tree()` now returns a list with the full COICOP tree at each level.
For backward compatibility, the new argument `flag=TRUE` can be used to still return a logical vector.


**New features and other changes:**

1. The function `contrib()` now allows to derive contributions to monthly, quarterly and annual change rates.
This can now also be done for quarterly and annual indices.

2. The function `convert()` now allows the calculation of a 12-month moving average.

3. The function `rebase()` now allows for multiple index reference periods in `t.ref` or setting this dynamically to the `first` or `last` period.

4. The functions `child()` and `parent()` now allow checking against the full COICOP code dictionary.
Moreover, the closest parents/children or the $k$-th parents/children can be derived.

5. The function `aggregate.tree()` relies on `parent()` and `level()`, which makes it more robust but slightly slower.

6. Added `settings$na.rm` for dealing with `NA`s when quarterly or yearly averages are computed.
For `na.rm=TRUE`, averages are now also calculated if there are `NA`s or less than 12 observations. 

7. Added `settings$freq` to set the frequency of the index series manually. 
Otherwise, the frequency is auto-detected.

8. Added `settings$chatty` for printing package specific warnings and messages.
This behavior can also be changed globally by `options("hicp.chatty")`.

9. Removed `settings$unbundle`.
COICOP codes are now always unbundled internally by the respective functions.
If that is not wanted by the user, the bundle codes can be set to `NA` or an empty list can be defined in `options(hicp.coicop.bundles)`.

10. Years and months are now extracted using the functions `data.table::year()` and `data.table::month()`, which is much faster.

11. All functions using dates now check internally for duplicates to avoid daily or weekly data. 

12. Added the argument `pattern="^prc_hicp"` to the function `datasets()`. 

13. Added the codes of the new ECOICOP version 2.


**Bug fixes:**

1. Fixed a bug in the functions `chain()` and `unchain()` when there was a gap of only one year and the index value was set to 100 in December of this year.
A break in the time series is now assumed if the calendar year is missing/`NA` or if only one index value is present in December (for `by=NULL`) or in the month specified with `by`.

2. Fixed a bug in `child()` for the derivation of children.


# hicp 0.6.1

1. Fixed a bug in the creation of the vignette on the `r-release-windows-x86_64`-flavor

# hicp 0.6.0

1. Introduced the global options `options("hicp.unbundle"=TRUE)`, `options(hicp.coicop.version="ecoicop-hicp")`, `options(hicp.all.items.code="00")`

2. The functions `is.coicop()`, `level()`, `parent()`, `child()`, `aggregate()`, and `tree()` now have the argument `settings`, which can be used to define a COICOP version (`coicop.version`), the all-items code (`all.items.code`), and how bundle codes are treated (`unbundle`)

3. The function `is.coicop()` now accepts different COICOP versions. The functions `level()`, `parent()`, `child()`, `aggregate()`, and `tree()` only process valid COICOP codes and the all-items code.

4. Fixed the behavior of `aggregate()` when the functions `jevons()`, `carli()`, and `harmonic()` are used and no weights `w0` are provided

5. Fixed a bug in the function `chain()`, which occurred when the unchained series started in January and `by=12`. In this case, the index reference period (with value 100) was not correctly detected

6. Updated documentation and tests accordingly

# hicp 0.5.1

1. Fixed a bug in the functions `jevons()`, `carli()`, and `harmonic()`, which checked the weights `w0` that were, however, not required by the function

2. Fixed a bug in the function `chain()`, which occurred when `NA`'s were present in the index where the index value was set to 100 (instead of `NA`)

3. Updated documentation and tests accordingly

# hicp 0.5.0

1. New functions `jevons()`, `carli()`, and `harmonic()` available

2. Function `aggregate()` now allows calculation of user-defined aggregates even if not all parts of an aggregate are present in the data (`settings$add.exact=FALSE`)

3. Function `aggregate()` no longer requires any weights `w0` or `wt` if the underlying `index` function does not require weights

# hicp 0.4.2

1. Function `link()` now returns a matrix instead of a data.table

2. Function `convert()` now returns a named vector instead of a data.table

3. Updated references to the new HICP Methodological Manual

4. Cleaned code for easier reference to other package functions

# hicp 0.4.1

1. Updated description

2. Replaced `\dontrun` with `\donttest` in examples

# hicp 0.4.0

1. Initial CRAN submission.

2. First Github release.
