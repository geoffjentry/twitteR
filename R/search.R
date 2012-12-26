Rtweets <- function(n=25, lang=NULL, since=NULL, ...) {
    searchTwitter("#rstats", n=n, lang=lang, since=since, ...)
}

searchTwitter <- function(searchString, n=25, lang=NULL,
                          since=NULL, until=NULL, locale=NULL,
                          geocode=NULL, sinceID=NULL, ...) {
    ## A basic search function.  Only implements a search on a string
    ## and will return n results
    if (n <= 0)
        stop("n must be positive")
    n <- as.integer(n)
    
    params <- buildCommonArgs(lang=lang, locale=locale, since=since, until=until,
                              geocode=geocode, since_id=sinceID)
    params[['q']] <- searchString
    jsonList <- doRppAPICall(n, params=params, ...)
    sapply(jsonList, buildStatus)
  }

