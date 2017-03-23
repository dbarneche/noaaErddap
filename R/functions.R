noaaErddapCacheDir  <-  function() {
    rappdirs::user_cache_dir('noaaErddap')
}

erddapLocal  <-  function (path, firstDay) {
    file.path(path, sprintf('%s.nc', format(firstDay, format = '%B-%Y')))
}

getFirstDayOfTheMonth  <-  function(month, year) {
    as.Date(paste0(year, '-', month, '-01'))
}

findLastDayOfTheMonth  <-  function(myDate) {
    # myDate: Date object (e.g. '2011-01-01')
    myDate        <-  as.POSIXct(as.character(myDate))
    dateLt        <-  as.POSIXlt(myDate) # add a month, then subtract a day:
    mon           <-  dateLt$mon + 2
    year          <-  dateLt$year
    year          <-  year + as.integer(mon == 13) # if month was December add a year
    mon[mon==13]  <-  1
    iso     <-  ISOdate(1900 + year, mon, 1, hour = 0, tz = attr(myDate, 'tz'))
    result  <-  as.POSIXct(iso) - 86400 # subtract one day
    result  <-  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst) * 3600
    format(result, format = '%Y-%m-%d')
}

productivityBaseLink  <-  function(firstDay) {
    lastDay  <-  findLastDayOfTheMonth(firstDay)
    sprintf('http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdPPbfp18day.nc?productivity[(%s):1:(%s)][(0.0):1:(0.0)][(-90.0):1:(90.0)][(0.0):1:(360.0)]', firstDay, lastDay)
}

chlorophyllBaseLink  <-  function(firstDay) {
    lastDay  <-  findLastDayOfTheMonth(firstDay)
    sprintf('http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdSW1chla8day.nc?chlorophyll[(%sT12:00:00Z):1:(%sT12:00:00Z)][(89.95834):1:(-89.95834)][(-179.9583):1:(179.9584)]', firstDay, lastDay)
}

sstBaseLink  <-  function(firstDay) {
    year  <-  format(firstDay, format = '%Y')
    sprintf('ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.%s.v2.nc', year)
}

erddapGet  <-  function(type, filePath, firstDay, overwrite, ...) {
    dir.create(dirname(filePath), showWarnings = FALSE, recursive = TRUE)
    typeLink  <-  get(paste0(type, 'BaseLink'))(firstDay)
    res       <-  httr::GET(typeLink, httr::write_disk(filePath, overwrite = overwrite), ...)
    httr::stop_for_status(res)
    res$request$output$path
}

checkDateLimits  <-  function(type, firstDay) {
    limits      <-  dataTypeLimits()
    limits      <-  limits[limits$type == type, ]
    if(!(firstDay >= limits$start & firstDay <= limits$end)) {
        stop(sprintf('Input date falls outside the boundaries (%s) of requested NOAA data', paste0(limits$start, ' -- ', limits$end)))
    }
}

dataTypeLimits  <-  function() {
    data.frame(type   =  c('productivity', 'chlorophyll', 'sst'),
               start  =  as.Date(c('1997-09-01', '1997-09-01', '1981-09-01')),
               end    =  c(as.Date(c('2010-12-31', '2010-12-31')), as.Date(paste0(format(Sys.Date(), format = '%Y-%m'), '-01'))),
               stringsAsFactors = FALSE
              )
}

#' Download NOAA ERDDAP Data.
#'
#' @title Download SST, NPP, and Chlorophyll Data from NOAA (ERDDAP)
#' @param year A numeric string (XXXX) informing the year.
#' @param month A numeric string informing the month (from 1 to 12).
#' @param type Type of environmental layer to be downloaded. Currently
#' takes 3 possible values: \code{'productivity'}, \code{'productivity'} and 
#' \code{'productivity'}. See \code{details} below.
#' @param overwrite Should the layer of interest be overwritten?
#' @param ... further arguments to \code{httr::GET}
#' @details Net Primary Productivity (NPP) data
#' from http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdPPbfp18day.html
#' NOAA ERDDAP > griddap > Data Access Form 
#' metadata in http://coastwatch.pfeg.noaa.gov/erddap/info/erdPPbfp18day/index.html 
#' Primary Productivity, SeaWiFS and Pathfinder, Global, 1997-2010, EXPERIMENTAL (8 Day Composite)
#' From 1997 - Nov - 29 to 2007 - Nov - 29
#' NPP is an 8-day composite product, so every consecutive eight days should produce an independent
#' time step. Note that this time series is not perfectly even. 
#' Data input should be in the format of year and month.
#' Chlorophyll-a (CHL) data
#' from http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdSW1chla8day.html
#' NOAA ERDDAP > griddap > Data Access Form 
#' metadata in http://coastwatch.pfeg.noaa.gov/erddap/info/erdSW1chla8day/index.html
#' Chlorophyll-a, Orbview-2 SeaWiFS, R2014.0, 0.1°, Global, 1997-2010 (8 Day Composite) 
#' From 1997 - Nov - 29 to 2007 - Nov - 29
#' Chlorophyll-a is an 8-day composite product, so every consecutive eight days should produce an independent
#' time step. Note that this time series is not perfectly even. 
#' Data input should be in the format of year and month.
#' Sea Surface Temperature (SST) data
#' from http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html
#' list of individual files available from:
#' http://www.esrl.noaa.gov/psd/cgi-bin/db_search/DBListFiles.pl?did=132&tid=55440&vid=2423
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
#' tabOfLinks  <-  ddply(dat, .(year, month), function(x, type)  {
#'     erddapDownload(x$year, x$month, type, overwrite = TRUE)
#' }, type = 'productivity')
#' # check outputs
#' pathToDownloadedFile; tabOfLinks
erddapDownload  <-  function(year, month, type, overwrite = FALSE, ...) {
    firstDay            <-  getFirstDayOfTheMonth(month, year)
    checkDateLimits(type, firstDay)
    path                <-  file.path(noaaErddapCacheDir(), type)
    filePath            <-  erddapLocal(path, firstDay)
    if(file.exists(filePath) & !overwrite) {
        warning('File already exists, please set "overwrite = TRUE" if you want to overwrite existing file')
    } else if(file.exists(filePath) & overwrite | !file.exists(filePath)) {
        cat('Attempting to download', filePath, '\n\n')
        erddapGet(type, filePath, firstDay, overwrite, ...)
    }
}
