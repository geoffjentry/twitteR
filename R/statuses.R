updateStatus <- function(text, ...) {
  if (!hasOAuth())
    stop("updateStatus requires OAuth authentication")
  url <- paste("http://twitter.com/statuses/update.json?status=",
               text, sep='')
  out <- doAPICall(url, method="POST")
  ## out is in byte code, need to pass that through a raw conversion
  ## to get the string.  Not sure why sometimes it's byte arrays and
  ## sometimes it is strings
  buildStatus(doAPICall(rawToChar(out)))
}

tweet <- function(text, ...) {
    ## Just a wrapper around updateStatus
    updateStatus(text, session, ...)
}

deleteStatus <- function(status, ...) {
  if (!hasOAuth())
    stop("deleteStatus requires OAuth authentication")
  url <- paste("http://twitter.com/statuses/destroy/",
                 status@id, ".json", sep="")
    ## I don't know how to simply POST or send a DELETE via RCurl w/o
    ## postForm, but this isn't a form so it throws a warning.
    ## Suppress these warnings
    out <- suppressWarnings(postForm(url, curl=session), ...)
    TRUE
}




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

homeTimeline <- function(n=25)
  authStatusBase(n, 'home_timeline')

friendsTimeline <- function(n=25)
  authStatusBase(n, 'friends_timeline')

mentions <- function(n=25)
  authStatusBase(n, 'mentions')

retweetedByMe <- function(n=25)
  authStatusBase(n, 'retweeted_by_me')

retweetedToMe <- function(n=25) 
  authStatusBase(n, 'retweeted_to_me')

retweetsOfMe <- function(n=25)
  authStatusBase(n, 'retweets_of_me')

authStatusBase <- function(n, type) {
  if (!hasOAuth())
    stop("OAuth is required for this functionality")

  n <- as.integer(n)
  if (n > 800) {
    warning("Maximum of 800 statuses can be returned, clipping")
    n <- 800
  }
  page <- 1
  total <- n
  count <- ifelse(n < 200, n, 200)
  jsonList <- list()
  while (total > 0) {
    url <- paste("http://api.twitter.com/1/statuses/",
                 type, ".json?count=", count,
                 '&page=', page, sep='')
    jsonList <- c(jsonList, doAPICall(url))
    total <- total - count
    page <- page + 1
  }
  if ((length(jsonList) > 0) && (length(jsonList) > n))
    jsonList <- jsonList[1:n]
  sapply(jsonList, buildStatus)
}



showStatus <- function(id, session=getCurlHandle(), ...) {
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

