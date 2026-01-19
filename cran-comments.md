## Release info

This is a minor resubmission fixing a few errors in the package tests, which resulted from an API non-response.
These errors occurred during the CRAN package checks.
All changes are documented in the `News.md` file.

## Test environments

* local Windows installation, R 4.4.3
* win-builder (devel, oldrelease, release) 

## R CMD check results

There were no ERRORs or WARNINGs. 

There were 3 NOTEs:
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

```
> Found the following (possibly) invalid URLs:
  URL: https://data.europa.eu/doi/10.2785/055028
    From: man/chaining.Rd
          man/index.aggregation.Rd
          man/rates.Rd
    Status: 404
    Message: Not Found
```
This DOI is valid and the URL works.

## Reverse dependencies

There are currently no reverse dependencies for this package.