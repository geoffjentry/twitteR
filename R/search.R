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

