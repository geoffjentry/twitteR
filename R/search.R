Rtweets <- function(n=25, session=getCurlHandle(), lang=NULL, since=NULL, ...) {
    searchTwitter("#rstats", n=n, session=session, lang=lang, since=since, ...)
}

searchTwitter <- function(searchString, n=25, 
                          session=getCurlHandle(), lang=NULL, since=NULL, ...) {
    ## A basic search function.  Only implements a search on a string
    ## and will return n results
    if (n <= 0)
        stop("n must be positive")
    n <- as.integer(n)
    qrySearch <- URLencode(searchString)
    if (nchar(qrySearch) > 140)
        stop("searchString is limited to 140 characters")
    jsonList <- list()

    
    batchSize <- ifelse(n < 100, n, 100)
    pageStr <- paste("?rpp=", batchSize,
                     "&page=1&q=",
                     qrySearch, sep='')
    if (! is.null(lang)) {
      pageStr <- paste(pageStr, '&lang=', lang, sep='')
    }
    if (! is.null(since)) {
      pageStr <- paste(pageStr, '&since=', since, sep='')
    }

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
