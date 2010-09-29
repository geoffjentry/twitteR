publicTimeline <- function(session=getCurlHandle(), ...) {
  jsonList <- doAPICall('http://api.twitter.com/1/statuses/public_timeline.json',
                        curl=session, ...)
    sapply(jsonList, buildStatus)
}

userTimeline <- function(user, n=20, session=getCurlHandle(), ...) {
    ## AUTH: Will not work if user is protected until OAuth
    if (inherits(user, "user"))
        user <- user@screenName
    n <- as.integer(n)
    if (n > 3200) {
        warning("userTimeline has a cap of 3200 statuses, clipping")
        n <- 3200
    }
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
        jsonList <- c(jsonList, doAPICall(url, curl=session, ...))
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
  buildStatus(doAPICall(url, ...))
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
    if (n <= 0)
        stop("n must be positive")
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
        json <- doAPICall(url, curl=session, ...)
        jsonList <- c(jsonList, json$users)
        cursor <- json[['next_cursor']]
    }
    ## Batches come in blocks of 100, so trim to what they requested
    ## if they requested a non-multiple of 100
    if (length(jsonList) > n)
        jsonList <- jsonList[1:n]
    sapply(jsonList, buildUser)
}

