#' Cache folder for ERDDAP datasets
#'
#' @title Creates cache folder to store ERDDAP datasets
#' @details Uses \link[rappdirs]{user_cache_dir} to create
#' a cache folder to store ERDDAP datasets
#' @author Diego Barneche.
#' @seealso \code{\link[noaaErddap]{erddapLocal}}.
noaaErddapCacheDir  <-  function() {
    rappdirs::user_cache_dir('noaaErddap')
}

#' Cache folder for ERDDAP datasets
#'
#' @title Creates cache folder to store ERDDAP datasets
#' @param path A folder path created via \link[noaaErddap]{noaaErddapCacheDir}.
#' @param firstDay An object of class \code{\link[base]{Date}} of format YYYY-MM-DD.
#' @return A file path.
#' @details Uses path created via \link[noaaErddap]{noaaErddapCacheDir} 
#' to create a full path in cache folder to store ERDDAP datasets.
#' @author Diego Barneche.
#' @seealso \code{\link[noaaErddap]{noaaErddapCacheDir}}.
erddapLocal  <-  function(path, firstDay) {
    wrongInput  <-  !is.character(path) | class(firstDay) != 'Date'
    if(wrongInput)
        stop('Input of wrong class')
    file.path(path, sprintf('%s.nc', format(firstDay, format = '%B-%Y')))
}

#' First day of the month
#'
#' @title Concatenates month and year to generate the first day of the month
#' @param year A \code{\link[base]{numeric}} or \code{\link[base]{character}} string (YYYY) informing the year.
#' @param month A \code{\link[base]{numeric}} (integer) or \code{\link[base]{character}} string (MM) informing the month (from 1 to 12).
#' @return An object of class \code{\link[base]{Date}} of format YYYY-MM-01.
#' @author Diego Barneche.
#' @seealso \code{\link[noaaErddap]{findLastDayOfTheMonth}}.
#' @export
getFirstDayOfTheMonth  <-  function(month, year) {
    as.Date(paste(year, month, '01', sep = '-'))
}

#' Last day of the month
#'
#' @title Finds the last day of the month
#' @param myDate An object of class \code{\link[base]{Date}}.
#' @return A \code{\link[base]{character}} vector of format YYYY-MM-DD.
#' @author Diego Barneche, code adapted from \href{https://www.r-bloggers.com/find-the-last-day-of-the-month/}{R-Bloggers}.
#' @seealso \code{\link[noaaErddap]{getFirstDayOfTheMonth}}.
#' @export
findLastDayOfTheMonth  <-  function(myDate) {
    myDate        <-  as.POSIXct(as.character(myDate))
    dateLt        <-  as.POSIXlt(myDate)
    # add a month, then subtract a day
    mon           <-  dateLt$mon + 2
    year          <-  dateLt$year
    # if month was December add a year
    year          <-  year + as.integer(mon == 13); mon[mon == 13]  <-  1 
    iso           <-  ISOdate(1900 + year, mon, 1, hour = 0, tz = attr(myDate, 'tz'))
     # subtract one day
    result        <-  as.POSIXct(iso) - 86400
    result        <-  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst) * 3600
    format(result, format = '%Y-%m-%d')
}

#' Base download link for productivity data
#'
#' @title Create a download link for productivity data
#' @param firstDay An object of class \code{\link[base]{Date}} of format YYYY-MM-DD.
#' @return An object of class \code{\link[base]{character}}.
#' @author Diego Barneche.
#' @seealso \code{\link[noaaErddap]{erddapGet}}.
productivityBaseLink  <-  function(firstDay) {
    lastDay  <-  findLastDayOfTheMonth(firstDay)
    sprintf('http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdPPbfp18day.nc?productivity[(%s):1:(%s)][(0.0):1:(0.0)][(-90.0):1:(90.0)][(0.0):1:(360.0)]', firstDay, lastDay)
}

#' Base download link for chlorophyll-a data
#'
#' @title Create a download link for chlorophyll-a data
#' @param firstDay An object of class \code{\link[base]{Date}} of format YYYY-MM-DD.
#' @return An object of class \code{\link[base]{character}}.
#' @author Diego Barneche.
#' @seealso \code{\link[noaaErddap]{erddapGet}}.
chlorophyllBaseLink  <-  function(firstDay) {
    lastDay  <-  findLastDayOfTheMonth(firstDay)
    sprintf('http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdSW1chla8day.nc?chlorophyll[(%sT12:00:00Z):1:(%sT12:00:00Z)][(89.95834):1:(-89.95834)][(-179.9583):1:(179.9584)]', firstDay, lastDay)
}

#' Base download link for SST data
#'
#' @title Create a download link for SST data
#' @param firstDay An object of class \code{\link[base]{Date}} of format YYYY-MM-DD.
#' @return An object of class \code{\link[base]{character}}.
#' @author Diego Barneche.
#' @seealso \code{\link[noaaErddap]{erddapGet}}.
sstBaseLink  <-  function(firstDay) {
    year  <-  format(firstDay, format = '%Y')
    sprintf('ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.%s.v2.nc', year)
}

#' Downloads data to package cache folder
#'
#' @title Create a download link for SST data
#' @param type Type of environmental layer. Currently
#' takes 3 possible values: \code{'productivity'}, \code{'chlorophyll'} and 
#' \code{'sst'}.
#' @param filePath The file path within the cached as created by \code{\link[noaaErddap]{noaaErddapCacheDir}}
#' @param firstDay An object of class \code{\link[base]{Date}} of format YYYY-MM-DD.
#' @param overwrite Should the layer of interest be overwritten?
#' @param ... further arguments to \code{\link[httr]{GET}}.
#' @return A character vector containing the path where files have been saved to.
#' @author Diego Barneche.
#' @seealso \code{\link[noaaErddap]{erddapDownload}}, \code{\link[noaaErddap]{noaaErddapCacheDir}}.
erddapGet  <-  function(type, filePath, firstDay, overwrite, ...) {
    dir.create(dirname(filePath), showWarnings = FALSE, recursive = TRUE)
    typeLink  <-  get(paste0(type, 'BaseLink'))(firstDay)
    res       <-  httr::GET(typeLink, httr::write_disk(filePath, overwrite = overwrite), ...)
    httr::stop_for_status(res)
    res$request$output$path
}

#' Check if input falls within Date limits
#'
#' @title Check if input falls within Date limits for currently supported ERDDAP data 
#' @param type Type of environmental layer. Currently
#' takes 3 possible values: \code{'productivity'}, \code{'chlorophyll'} and 
#' \code{'sst'}. See Details below.
#' @param firstDay An object of class \code{\link[base]{Date}} of format YYYY-MM-01.
#' @details The function uses internal \code{\link[noaaErddap]{dataTypeLimits}} to
#' evaluate whether the provided month and year in \code{\link[noaaErddap]{erddapDownload}}
#' follow within the supported dates. If YYYY-MM is identical to either beginning  
#' or end of supported range, than the function modifies \code{firstDay} to have the same
#' day as the starting (or ending) supported date.
#' @return An object of class \code{\link[base]{Date}} of format 'YYYY-MM-DD'
#' currently supported ERDDAP data 
#' @author Diego Barneche.
#' @seealso \code{\link[noaaErddap]{dataTypeLimits}}, \code{\link[noaaErddap]{erddapDownload}}.
checkDateLimits  <-  function(type, firstDay) {
    limits      <-  dataTypeLimits()
    limits      <-  limits[limits$type == type, ]
    strYearMon  <-  lapply(c(limits$start, limits$end, firstDay), zoo::as.yearmon)
    if (!(strYearMon[[3]] >= strYearMon[[1]] & strYearMon[[3]] <= strYearMon[[2]])) {
        stop(sprintf('Input date falls outside the boundaries (%s) of requested NOAA data', paste0(limits$start, ' -- ', limits$end)))
    }
    if (strYearMon[[3]] == strYearMon[[1]]) {
        firstDay  <-  limits$start
    } else if (strYearMon[[3]] == strYearMon[[2]]) {
        firstDay  <-  limits$end
    }
    firstDay
}

#' Date limits for currently supported ERDDAP data 
#'
#' @title Date limits for currently supported ERDDAP data
#' @return A \code{\link[base]{data.frame}} showing the Date limits 
#' currently supported ERDDAP data 
#' @author Diego Barneche.
#' @export
dataTypeLimits  <-  function() {
    data.frame(type   =  c('productivity', 'chlorophyll', 'sst'),
               start  =  as.Date(c('1997-09-10', '1997-09-02', '1981-09-01')),
               end    =  c(as.Date(c('2010-12-07', '2010-12-15')), as.Date(paste0(format(Sys.Date(), format = '%Y-%m'), '-01'))),
               stringsAsFactors = FALSE
              )
}

#' Download NOAA ERDDAP Data.
#'
#' @title Download SST, NPP, and Chlorophyll Data from NOAA (ERDDAP)
#' @param year A \code{\link[base]{numeric}} or \code{\link[base]{character}} string (YYYY) informing the year.
#' @param month A \code{\link[base]{numeric}} (integer) or \code{\link[base]{character}} string (MM) informing the month (from 1 to 12).
#' @param type Type of environmental layer to be downloaded. Currently
#' takes 3 possible values: \code{'productivity'}, \code{'chlorophyll'} and 
#' \code{'sst'}. See \code{details} below.
#' @param overwrite Should the layer of interest be overwritten?
#' @param ... further arguments to \code{\link[httr]{GET}}.
#' @details Net Primary Productivity (NPP) data
#' from \url{http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdPPbfp18day.html}
#' NOAA ERDDAP > griddap > Data Access Form 
#' metadata in \url{http://coastwatch.pfeg.noaa.gov/erddap/info/erdPPbfp18day/index.html}.
#' Primary Productivity, SeaWiFS and Pathfinder, Global, 1997-2010, EXPERIMENTAL (8 Day Composite)
#' From 1997 - Nov - 29 to 2007 - Nov - 29
#' NPP is an 8-day composite product, so every consecutive eight days should produce an independent
#' time step. Note that this time series is not perfectly even. 
#' Data input should be in the format of year and month.
#'
#' Chlorophyll-a (CHL) data
#' from \url{http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdSW1chla8day.html}
#' NOAA ERDDAP > griddap > Data Access Form 
#' metadata in \url{http://coastwatch.pfeg.noaa.gov/erddap/info/erdSW1chla8day/index.html}
#' Chlorophyll-a, Orbview-2 SeaWiFS, R2014.0, 0.1°, Global, 1997-2010 (8 Day Composite) 
#' From 1997 - Nov - 29 to 2007 - Nov - 29
#' Chlorophyll-a is an 8-day composite product, so every consecutive eight days should produce an independent
#' time step. Note that this time series is not perfectly even. 
#' Data input should be in the format of year and month.
#'
#' Sea Surface Temperature (SST) data
#' from \url{http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html}
#' Brief Description: NOAA High-resolution Blended Analysis of Daily SST and Ice. 
#' Data is from Sep 1981 and is on a 1/4 deg global grid.
#' Temporal Coverage: Daily values from 1981/09 to present
#' Spatial Coverage: 0.25 degree latitude x 0.25 degree longitude global grid (1440x720).
#' 89.875S - 89.875N,0.125E to 359.875E.
#' Main ref: Reynolds, Richard W., Thomas M. Smith, Chunying Liu, Dudley B. Chelton, Kenneth S. Casey, Michael G. Schlax, 2007: Daily H'igh-Resolution-Blended Analyses for Sea Surface Temperature. J. Climate, 20, 5473-5496. Reynolds, Richard W., Thomas M. Smith, Chunying Liu, Dudley B. Chelton, Kenneth S. Casey, Michael G. Schlax, 2007: Daily High-Resolution-Blended Analyses for Sea Surface Temperature. J. Climate, 20, 5473-5496.
# Not that different from NPP and CHL, SST is provided on a yearly basis.
# Thus, inputs such as \code{erddapDownload(year = 2006, month = 1, type = 'productivity')} and
# \code{erddapDownload(year = 2006, month = 10, type = 'productivity')} will yield the same layer.
# This happens because internal methods ignore the month argument to download SST layers downstream.
#' @return A character vector containing the path where files have been saved to.
#' @author Diego Barneche.
#' @export
#' @examples
#' # download productivity data for Jan 2006
#' pathToDownloadedFile  <-  erddapDownload(year = 2006, month = 1, type = 'productivity')
#' # download productivity data for all months between 1998-2008
#' library(plyr)
#' dat  <-  expand.grid(year = 1998:2008, month = 1:12)
#' tableOfPaths  <-  ddply(dat, .(year, month), function(x, type)  {
#'     erddapDownload(x$year, x$month, type, overwrite = TRUE)
#' }, type = 'productivity')
#' # check outputs
#' pathToDownloadedFile; tableOfPaths
erddapDownload  <-  function(year, month, type, overwrite = FALSE, ...) {
    firstDay  <-  getFirstDayOfTheMonth(month, year)
    firstDay  <-  checkDateLimits(type, firstDay)
    path      <-  file.path(noaaErddapCacheDir(), type)
    filePath  <-  erddapLocal(path, firstDay)
    if (file.exists(filePath) & !overwrite) {
        warning(sprintf('File %s already exists, please set "overwrite = TRUE" if you want to overwrite existing file', filePath))
        filePath
    } else if (file.exists(filePath) & overwrite | !file.exists(filePath)) {
        cat('Attempting to download', filePath, '\n\n')
        erddapGet(type, filePath, firstDay, overwrite, ...)
    }
}
