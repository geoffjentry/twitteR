Rtweets <- function(n=25, session=getCurlHandle(), ...) {
    searchTwitter("#rstats", n, session, ...)
}

searchTwitter <- function(searchString, n=25,
                          session=getCurlHandle(), ...) {
    ## A basic search function.  Only implements a search on a string
    ## and will return n results
    if (n <= 0)
        stop("n must be positive")
    n <- as.integer(n)
    qrySearch <- URLencode(searchString)
    if (nchar(qrySearch) > 140)
        stop("searchString is limited to 140 characters")
    page <- 1
    jsonList <- list()
    batchSize <- 100
    if (n > batchSize)
        reqLength <- batchSize
    curDiff <- n
    while (curDiff > 0) {
        if (curDiff < batchSize)
            reqLength <- curDiff
        url <- paste("http://search.twitter.com/search.json?q=",
                     qrySearch, "&rpp=", curDiff,
                     "&page=", page, '&result_type=recent',
                     sep="")
        out <- getURL(url, ...)
        newList <- twFromJSON(out)[[1]]
        if (length(newList) == 0) {
            ## for one reason or another that's not API limit, we've
            ## been cut off
            break
        } else {
            jsonList <- c(jsonList, newList)
            curDiff <- n - length(jsonList)
            page <- page + 1
        }
    }
    sapply(jsonList, buildStatus)
}
