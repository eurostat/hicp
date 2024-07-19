## Resubmission
This is a submission of an updated package version. In this version I have:

* Introduced the global options `options("hicp.unbundle"=TRUE)`, `options(hicp.coicop.version="ecoicop-hicp")`, `options(hicp.all.items.code="00")`
* The functions `is.coicop()`, `level()`, `parent()`, `child()`, `aggregate()`, and `tree()` now have the argument `settings`, which can be used to define a COICOP version (`coicop.version`), the all-items code (`all.items.code`), and how bundle codes are treated (`unbundle`)
* The function `is.coicop()` now accepts different COICOP versions. The functions `level()`, `parent()`, `child()`, `aggregate()`, and `tree()` only process valid COICOP codes and the all-items code.
* Fixed the behavior of `aggregate()` when the functions `jevons()`, `carli()`, and `harmonic()` are used and no weights `w0` are provided
* Fixed a bug in the function `chain()`, which occurred when the unchained series started in January and `by=12`. In this case, the index reference period (with value 100) was not correctly detected
* Updated documentation and tests accordingly

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

This seems to be related to the test environment and can be ignored.

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

This seems to be related to the test environment and can be ignored.

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

This seems to be related to the test environment and can be ignored.

0 errors ✔ | 0 warnings ✔ | 3 notes ✖
