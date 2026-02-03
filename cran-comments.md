## Release info

This is a minor release adding a few new functions and features to existing functions and fixing a bug.
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