availableTrendLocations = function(...) {
  locs = twInterfaceObj$doAPICall("trends/available", ...)
  return(importTrends(locs))
}

closestTrendLocations = function(lat, long, ...) {
  params=list(lat=lat, long=long)
  
  locs = twInterfaceObj$doAPICall("trends/closest", params=params, ...)
  return(importTrends(locs))
}

buildTrendLocationDf = function(loc_json) {
  loc_rows = lapply(loc_json, function(x) {
    return(c(name=x$name, country=x$country, woeid=x$woeid))
  })
  return(as.data.frame(do.call(rbind, loc_rows), stringsAsFactors=FALSE))
  
}

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
