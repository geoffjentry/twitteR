Rtweets <- function(n=25, lang=NULL, since=NULL, ...) {
    searchTwitter("#rstats", n=n, lang=lang, since=since, ...)
}

searchTwitter <- function(searchString, n=25, lang=NULL,
                          since=NULL, until=NULL, locale=NULL,
                          geocode=NULL, sinceID=NULL, 
                          retryOnRateLimit=120, ...) {

  
  if (nchar(searchString) > 1000) {
    stop("searchString can only be up to 1000 characters")
  }
  
  if (n <= 0)
        stop("n must be positive")
    n <- as.integer(n)
  
  if (is.null(since)) {
    since_date = NULL
  } else {
    since_date = strsplit(since, " ")[[1]][1]
  }
  if (is.null(until)) {
    until_date = NULL
  } else {
    until_date = strsplit(until, " ")[[1]][1] 
    if (until_date == since_date) {
      ## If since & until are on the same day nothing will be returned. Move
      ## until up a day and then we'll filter this later
      until_date = as.Date(since_date) + 1
    }
  }

  if (!is.null(geocode)) {
    geocheck = strsplit(geocode[[1]], ',')
    lon = as.numeric(geocheck[[1]])
    lat = as.numeric(geocheck[[2]])
    if ((lon > 180) || (lon < -180)) {
      stop('Longitude neet to be in range [180,-180].')
    }
    if ((lat > 90)||(lat < -90)) {
      stop('Latitude need to be in range [90.0,-90.0].')
    }
  }

  params <- buildCommonArgs(lang=lang, locale=locale, since=since_date, until=until_date,
                            geocode=geocode, since_id=sinceID)
  params[['q']] <- searchString
  jsonList <- doRppAPICall("search/tweets", n, params=params, retryOnRateLimit=retryOnRateLimit, ...)
  statuses = sapply(jsonList, buildStatus)
  
  datetimes = sapply(statuses, function(x) x$getCreated())
  if (is.null(since)) {
    since_statuses = seq_along(statuses)
  } else {
    since_statuses = which(datetimes >= as.numeric(as.POSIXct(since, tz="UTC")))
  }
  if (is.null(until)) {
    until_statuses = seq_along(statuses)
  } else {
    until_statuses = which(datetimes <= as.numeric(as.POSIXct(until, tz="UTC")))
  }
  good_statuses = intersect(since_statuses, until_statuses)
  return(statuses[good_statuses])
}

