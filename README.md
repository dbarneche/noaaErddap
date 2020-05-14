noaaErddap
==========

[![Build Status](https://api.travis-ci.org/dbarneche/noaaErddap.png?branch=master)](https://travis-ci.org/dbarneche/noaaErddap)
[![lifecycle](https://img.shields.io/badge/lifecycle-retired-orange.svg)](https://www.tidyverse.org/lifecycle/#retired)

**WARNING:** Please note, this package was experimental and is no longer being developed. If you decide to use it, please be aware that no support is given. I recommend using the rOpenSci [rerddap](https://docs.ropensci.org/rerddap/) instead.

`noaaErddap` is an R interface to three NOAA marine data sources. The package is concerned with downloading the data and providing two methods to extract methods given a `data.frame` of geographical coordinates.

## Data sources in noaaErddap

* [Net Primary Productivity (NPP) data](http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdPPbfp18day.html)
* [Chlorophyll-a (CHL) data](http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdSW1chla8day.html)
* [Sea Surface Temperature (SST)](http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html)

## netcdf data

Functions to manipulate and extract environmental data need netCDF support. You'll need both the `ncdf4` and `raster` packages. Installation of `ncdf4` should be straightforward on Mac and Windows, but on Linux you may have issues.

## NOAA ERDDAP Datasets time coverage

You can use the function `noaaErddap::dataTypeLimits` to visualise this dataset within R.


|Dataset       |Description     |Lat Range |Lon Range   |Start Date |End Date   |
|:-------------|:---------------|----------|------------|:----------|:----------|
|NPP           |8 Day Composite |-90 to 90 |0 to 360    |1997-09-10 |2010-12-07 |
|Chlorophyll-a |8 Day Composite |-90 to 90 |-180 to 180 |1997-09-02 |2010-12-15 |
|SST           |1 Day Composite |-90 to 90 |0 to 360    |1981-09-01 |Present    |

## Installation

The `noaaErddap` package can be installed from github using the [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html) package using `devtools::install_github`.

If you do not yet have `devtools`, install with `install.packages("devtools")`.

Then install `noaaErddap` using the following:  
`library(devtools)`  
`install_github('dbarneche/noaaErddap')`  
`library(noaaErddap)`

## Examples

```
library(noaaErddap)

# check help page for main function
?noaaErddap::erddapDownload

# downloads chlorophyll data for Jan/2006
linkToCachedFile  <-  noaaErddap::erddapDownload(year = 2006, month = 1, type = 'chlorophyll', overwrite = TRUE)
library(ncdf4)
ncdf4::nc_open(filename = linkToCachedFile)

# downloads productivity data for all months in 1998 (might take a few hours)
library(plyr)
dat  <-  data.frame(year = 1998, month = 1:12)
nppFiles  <-  plyr::ddply(dat, .(year, month), function (x, type) {
	data.frame(links = noaaErddap::erddapDownload(x$year, x$month, type, overwrite = TRUE), stringsAsFactors = FALSE)
}, type = 'productivity')

# get longitude and latitude for NPP files
envNc      <-  ncdf4::nc_open(filename = nppFiles$links[1])
longitude  <-  envNc$var[['productivity']]$dim[[1]]$vals
latitude   <-  envNc$var[['productivity']]$dim[[2]]$vals
ncdf4::nc_close(envNc)

# You can always recover cached files in a new session, e.g.
nppFiles  <-  noaaErddap::noaaErddapFiles('productivity', full.name = TRUE)

# extract NPP values for a given subset of coordinates (median value within a buffer of 20 km) across all files and take the mean
library(raster)
library(abind)
nppValues  <-  abind::abind(lapply(nppFiles, noaaErddap::openAndMatchNcdfData, method = 'raster', coordinates = data.frame(Longitude = c(330, 335, 340), Latitude = c(-27, -19, 0)), buffer = 2e4, fun = median, na.rm = TRUE), along = 3)
apply(nppValues, c(1, 2), mean, na.rm = TRUE)

# extract the average NPP values for the globe in 1998 (these steps are memory intensive)
nppValues1998  <-  abind::abind(lapply(nppFiles[grep('-1998.nc', nppFiles, fixed = TRUE)], noaaErddap::openAndMatchNcdfData, method = 'ncdf4'), along = 3)
meanNPP1998    <-  apply(nppValues1998, c(1, 2), mean, na.rm = TRUE)
```

## Acknowledgements

I'd like to thank the [rOpenSci project](https://ropensci.org/) for providing source code via the [`rnoaa` package](https://github.com/ropensci/rnoaa) from which I based this package.

## Bug reporting

Please [report any issues or bugs](https://github.com/dbarneche/noaaErddap/issues).
