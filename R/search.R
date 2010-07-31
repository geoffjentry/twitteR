Rtweets <- function(session=getCurlHandle(), num=25, ...) {
    searchTwitter("#rstats", session, num, ...)
}

searchTwitter <- function(searchString, session=getCurlHandle(),
                          n=25, ...) {
    ## A basic search function.  Only implements a search on a string
    ## and will return n results
    if (n <= 0)
        stop("n must be positive")
    n <- as.integer(n)
    qrySearch <- URLencode(searchString)
    if (nchar(qrySearch) > 140)
        stop("searchString is limited to 140 characters")
    page <- 1
    total <- n
    if (n > 100)
        n <- 100
    jsonList <- list()
    while (total > 0) {
        url <- paste("http://search.twitter.com/search.json?q=",
                     qrySearch, "&rpp=", n,
                     "&page=", page, sep="")
        out <- getURL(url, ...)
        jsonList <- c(jsonList, twFromJSON(out)[[1]])
        total <- total - n
        page <- page + 1
    }
    sapply(jsonList, buildStatus)
}
