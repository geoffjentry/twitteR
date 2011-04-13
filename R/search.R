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
    qrySearch <- searchString
    if (nchar(qrySearch) > 140)
        stop("searchString is limited to 140 characters")
    jsonList <- list()
    
    batchSize <- ifelse(n < 100, n, 100)
    pageStr <- paste("?rpp=", batchSize,
                     "&page=1&q=",
                     qrySearch, sep='')

    for (arg in c('lang', 'locale', 'since', 'until', 'geocode')) {
      val <- get(arg)
      if (!is.null(val))
        pageStr <- paste(pageStr, '&', arg, '=', val, sep='')
    }
    ## do sinceID separately as formatting is different
    if (!is.null(sinceID))
      pageStr <- paste(pageStr, '&since_id=', sinceID, sep='')
    
    curDiff <- n
    while (curDiff > 0) {
        url <- paste("http://search.twitter.com/search.json",
                     pageStr, "&result_type=recent",
                     sep="")
        fromJSON <- doAPICall(url, ...)
        newList <- fromJSON$results
        jsonList <- c(jsonList, newList)
        curDiff <- n - length(jsonList)
        if (curDiff > 0) {
            if ("next_page" %in% names(fromJSON)) {
                pageStr <- fromJSON$"next_page"
                if (curDiff < 100) {
                    ## We don't want to get a full 100 results, replace
                    ## what twitter tells us with our curDiff
                    pageStr <- sub("rpp=[[:alnum:]]+",
                                   paste("rpp", curDiff, sep="="),
                                   pageStr)
                }

            } else {
                break
            }
        }
    }
    sapply(jsonList, buildStatus)
}
