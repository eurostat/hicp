## Resubmission

This is a resubmission, in which I replaced `\url{}` with `\doi{}` for citing the HICP Manual.
The first submission (see below) let the CRAN checks indicate an invalid URL due to the use of `\url{}`.


## Release info

This is a patch release fixing a few errors in the package tests, which resulted from an API non-response.
These errors occurred during the CRAN package checks.
All changes are documented in the `News.md` file.


## Test environments

* local Windows installation, R 4.4.3
* win-builder (devel, oldrelease, release) 


## R CMD check results

There were no ERRORs or WARNINGs. 

There were 2 NOTEs:
```
❯ checking for future file timestamps ... NOTE
  unable to verify current time
```
This note seems to appear from time to time only, possibly due to some unavailability of the underlying API.

```
❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
```
This seems to be related to the test environment and can be ignored.


## Reverse dependencies

There are currently no reverse dependencies for this package.