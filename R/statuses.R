publicTimeline <- function(session=getCurlHandle(), ...) {
    out <- getURL("http://api.twitter.com/1/statuses/public_timeline.json",
                  curl=session, ...)
    jsonList <- twFromJSON(out)
    sapply(jsonList, buildStatus)
}

userTimeline <- function(user, n=20, session=getCurlHandle(), ...) {
    ## AUTH: Will not work if user is protected until OAuth
    if (inherits(user, "user"))
        user <- user@screenName
    n <- as.integer(n)

    page <- 1
    total <- n
    if (n > 200)
        n <- 200
    jsonList <- list()
    while (total > 0) {
        url <- paste("http://api.twitter.com/1/statuses/",
                     "user_timeline.json?screen_name=",
                     URLencode(user), '&count=', n,
                     '&page=', page, sep="")

        out <- getURL(url, curl=session, ...)
        jsonList <- c(jsonList, twFromJSON(out))
        total <- total - n
        page <- page + 1
    }
    sapply(jsonList, buildStatus)
}

showStatus <- function(id, session=getCurlHandle(), ...) {
    ## Doesn't require authentication via initSession unless the owner
    ## of the status is protected
    if (!is.numeric(id))
        stop("id argument must be numeric")
    url <- paste("http://api.twitter.com/1/statuses/show/",
                        URLencode(as.character(id)), ".json", sep="")
    out <- getURL(url, ...)
    ret <- twFromJSON(out)
    buildStatus(ret)
}

userFriends <- function(user, n=100, session=getCurlHandle(), ...) {
    ffBase(user, 'friends', n, session, ...)
}

userFollowers <- function(user, n=100, session=getCurlHandle(), ...) {
    ffBase(user, 'followers', n, session, ...)
}

ffBase <- function(user, type, n=100,
                         session=getCurlHandle(), ...) {
    if (inherits(user, 'user'))
        user <- screenName(user)
    n <- as.integer(n)
    baseUrl <- paste('http://api.twitter.com/1/statuses/',
                     type, '/', URLencode(user), '.json?cursor=', sep='')
    pages <- ceiling(n/100)
    jsonList <- list()
    cursor = -1
    for (i in seq(to=pages)) {
        ## Probably shouldn't be catting multiple lists together,
        ## although this does need to be done sequentially as each
        ## call gets the cursor for the next call
        url <- paste(baseUrl, cursor, sep='')
        out <- getURL(url, curl=session, ...)
        json <- twFromJSON(out)
        jsonList <- c(jsonList, json$users)
        cursor <- json[['next_cursor']]
    }
    sapply(jsonList, buildUser)
}

