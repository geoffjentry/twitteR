availableTrendLocations = function(...) {
  locs = twInterfaceObj$doAPICall("trends/available", ...)
  return(import_trends(locs))
}

closestTrendLocations = function(lat, long, ...) {
  params=list(lat=lat, long=long)
  
  locs = twInterfaceObj$doAPICall("trends/closest", params=params, ...)
  return(import_trends(locs))
}

buildTrendLocationDf = function(loc_json) {
  loc_rows = lapply(loc_json, function(x) {
    return(c(name=x$name, country=x$country, woeid=x$woeid))
  })
  return(as.data.frame(do.call(rbind, loc_rows), stringsAsFactors=FALSE))
  
}



#' Functions to view Twitter trends
#' 
#' These functions will allow you to interact with the trend portion of the
#' Twitter API
#' 
#' The \code{availableTrendLocations} and \code{closestTrendLocations}
#' functions will return a data.frame with three columns - \code{name},
#' \code{country} and \code{woeid}. The \code{closestTrendLocations} function
#' will return the locations closest to the specified latitude and longitude.
#' 
#' The \code{getTrends} function takes a specified woeid and returns the
#' trending topics associated with that woeid. It returns a data.frame with the
#' columns being \code{name}, \code{url}, \code{promoted_content}, \code{query}
#' and \code{woeid} - one row per trend.
#' 
#' @aliases getTrends availableTrendLocations closestTrendLocations
#' @param woeid A numerical identification code describing a location, a Yahoo!
#' Where On Earth ID
#' @param lat A numerical latitude value, between -180 and 180 inclusive. West
#' is negative, East is positive
#' @param long A numerical longitude value, between -180 and 180 inclusive.
#' South is negative, North is positive
#' @param exclude If set to \code{hashtags}, will exclude hashtags
#' @param ... Additional arguments to be passed to RCurl
#' @return A data.frame with the columns specified in \code{Details} above
#' @author Jeff Gentry
#' @keywords interface
#' @examples
#' 
#'   \dontrun{
#'     woeid = availableTrendLocations[1, "woeid"]
#'     t1 <- getTrends(woeid)
#'   }
#' 
getTrends <- function(woeid, exclude=NULL, ...) {
  params <- buildCommonArgs(exclude=exclude)
  params[["id"]] = woeid
  jsonList <- twInterfaceObj$doAPICall("trends/place", params=params, ...)
  trends <- jsonList[[1]][['trends']]
  trend_rows = lapply(trends, function(x) {
    return(c(name=x$name, url=x$url, promoted_content=x$promoted_content,
             query=x$query, events=x$events, woeid=woeid))
  })
  return(as.data.frame(do.call(rbind, trend_rows), stringsAsFactors=FALSE))
}
