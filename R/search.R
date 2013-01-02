Rtweets <- function(n=25, lang=NULL, since=NULL, ...) {
    searchTwitter("#rstats", n=n, lang=lang, since=since, ...)
}

searchTwitter <- function(searchString, n=25, lang=NULL,
                          since=NULL, until=NULL, locale=NULL,
                          geocode=NULL, sinceID=NULL, ...) {

  if (nchar(searchString) > 1000) {
    stop("searchString can only be up to 1000 characters")
  }

  
  if (n <= 0)
        stop("n must be positive")
    n <- as.integer(n)
    
    params <- buildCommonArgs(lang=lang, locale=locale, since=since, until=until,
                              geocode=geocode, since_id=sinceID)
    params[['q']] <- searchString
    jsonList <- doRppAPICall(n, params=params, ...)
    sapply(jsonList, buildStatus)
  }

