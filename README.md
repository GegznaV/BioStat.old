
<!-- README.md is generated from README.Rmd. Please edit that file -->

------------------------------------------------------------------------

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/BioStat)](https://cran.r-project.org/package=BioStat) [![GitHub version](https://img.shields.io/badge/GitHub-v0.0.9-brightgreen.svg)](https://github.com/GegznaV/BioStat) [![Travis-CI Build Status](https://travis-ci.org/GegznaV/BioStat.png?branch=master)](https://travis-ci.org/GegznaV/BioStat) [![codecov.io](https://codecov.io/github/GegznaV/BioStat/coverage.svg?branch=master)](https://codecov.io/github/GegznaV/BioStat?branch=master) [![Update-on](https://img.shields.io/badge/Updated%20on-2017--11--26-yellowgreen.svg)](/commits/master)

------------------------------------------------------------------------

<img src="https://raw.githubusercontent.com/GegznaV/BioStat/master/docs/logo.png" width="30%" height="30%" style="display: block; margin: auto;" />

`BioStat` – A Collection of Functions for Biostatistics and Biometry Lectures
=============================================================================

`BioStat` is an `R` package that contains a collection of functions that either are intended to be used with R Commander plugin *RcmdrPlugin.BioStat* or to do several common statistical routines without writing to much code. The functions are created to complement other R Commander plugins that can be used to teach basic statistics in biostatistics and biometry lectures.

The package is still in its development and some functions are for demonstration purposes only as they may change in the future.

Documentation and more information available at <http://gegznav.github.io/BioStat/>

Install package
---------------

To install a released version of the package from *CRAN*:

``` r
install.packages("BioStat")
```

To install a developement version of the package from *GitHub*:

``` r
if (!"devtools" %in% installed.packages()) 
    install.packages("devtools")

devtools::install_github("GegznaV/BioStat")
```

------------------------------------------------------------------------

Other related packages
======================

Other related packages are *R Commander* (*Rcmdr*) plugins:

1.  **RcmdrPlugin.BioStat** -- an *R Commander* plugin for `BioStat` package ([homepage](https://gegznav.github.io/RcmdrPlugin.BioStat/));
2.  **RcmdrPlugin.EZR.2** -- an *R Commander* plugin for the most common statistical analyses (the same as *RcmdrPlugin.EZR*, except that *RcmdrPlugin.EZR.2* does not modify original *Rcmdr* menu so dramatically);
3.  **RcmdrPlugin.KMggplot2** -- an *R Commander* plugin for *ggplot2* graphics.

To install these packages, use the following code:

``` r
# RcmdrPlugin.BioStat
devtools::install_github("GegznaV/RcmdrPlugin.BioStat")

# RcmdrPlugin.EZR.2
devtools::install_github("GegznaV/RcmdrPlugin.EZR@unmodified_Rcmdr_menu")

# RcmdrPlugin.KMggplot2
install.packages("RcmdrPlugin.KMggplot2")
```

<!--  <p align="right"> </p>     -->

------------------------------------------------------------------------
