noaaErddap
==========



[![Build Status](https://api.travis-ci.org/dbarneche/noaaErddap.png?branch=master)](https://travis-ci.org/dbarneche/noaaErddap)

`noaaErddap` is an R interface to three NOAA marine data sources. The package is concerned with downloading the data and providing two methods to extract methods given a `data.frame` of geographical coordinates.

## Data sources in noaaErddap

* [Net Primary Productivity (NPP) data](http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdPPbfp18day.html)
* [Chlorophyll-a (CHL) data](http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdSW1chla8day.html)
* [Sea Surface Temperature (SST)](http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html)

## netcdf data

Functions to manipulate and extract environmental data need netCDF support. You'll need both the `ncdf4` and `raster` packages. Installation of `ncdf4` should be straightforward on Mac and Windows, but on Linux you may have issues.

## NOAA ERDDAP Datasets time coverage

You can use the function `noaaErddap::dataTypeLimits` to visualise this dataset within R.


|Dataset       |Description     |Start Date |End Date   |
|:-------------|:---------------|:----------|:----------|
|NPP           |8 Day Composite |1997-09-10 |2010-12-07 |
|Chlorophyll-a |8 Day Composite |1997-09-02 |2010-12-15 |
|SST           |1 Day Composite |1981-09-01 |Present    |

## Installation

The `noaaErddap` package can be installed from github using the [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html) package using `devtools::install_github`.

If you do not yet have `devtools`, install with `install.packages("devtools")`.

Then install `noaaErddap` using the following:  
`library(devtools)`  
`install_github('dbarneche/noaaErddap')`  
`library(noaaErddap)`

## Acknowledgements

I'd like to thank the [rOpenSci project](https://ropensci.org/) for providing source code via the [`rnoaa` package](https://github.com/ropensci/rnoaa) from which I based this package.

## Bug reporting

* Please [report any issues or bugs](https://github.com/dbarneche/noaaErddap/issues).
