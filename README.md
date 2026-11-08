# robjc

[日本語](README.ja.md)

`robjc` is an Objective-C inspired object framework for R, built on R6.
It provides class factories, protocols, scalar wrappers, mutable and immutable
collections, and archiving for object graphs. It is intended for applications
that manage connected domain objects and need explicit identity, equality, and
hashing rules.

## Installation

The development version is available from GitHub. Once the package is on
CRAN, it can also be installed with `install.packages("robjc")`.

```r
install.packages("remotes")  # If remotes is not installed
remotes::install_github("YanoSatoshi/RObjC")
```

## Quick example

Collections store `robjc` objects. Wrap ordinary R values in scalar objects
before inserting them. Dictionary keys are looked up using `hash()` and
`isEqual()`.

```r
library(robjc)

items <- RSMutableArray$new()
items$addObject(RSString$new("Ada"))
items$addObject(RSNumber$new(42L))
items$count()
#> [1] 2

lookup <- RSMutableDictionary$new()
lookup$setObjectForKey(RSNumber$new(42L), RSString$new("answer"))
lookup$objectForKey(RSString$new("answer"))$value()
#> [1] 42

saved_items <- items$copy()  # Immutable RSArray
```

Custom objects used as Dictionary keys or Set members must implement
compatible `isEqual()` and `hash()` methods. A hash must remain stable while
the object is stored in a hash collection. `NULL` is not inserted
automatically; use `RSNull$null` when a null object is needed.

## Documentation

- [English vignette contents](vignettes/contents.Rmd)
- [Quick Start](vignettes/guide_01_QuickStart.Rmd)
- [Collections](vignettes/guide_07_Collections.Rmd)
- [Archiving](vignettes/guide_10_Archiving.Rmd)

Please report bugs and feature requests in [GitHub Issues](https://github.com/YanoSatoshi/RObjC/issues).
