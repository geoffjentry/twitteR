updateStatus <- function(text, lat=NULL, long=NULL, placeID=NULL,
                         displayCoords=NULL, inReplyTo=NULL) {
  if (!hasOAuth())
    stop("updateStatus requires OAuth authentication")

  if (nchar(text) > 140)
    stop("Status can not be more than 140 characters")

  url <- paste("http://api.twitter.com/1/statuses/update.json?status=",
               text, sep='')
  if (!is.null(lat))
    url <- paste("&lat=", lat, sep='')
  if (!is.null(long))
    url <- paste("&long=", long, sep='')
  if (!is.null(placeID))
    url <- paste("&place_id=", placeID, sep='')
  if (!is.null(displayCoords))
    url <- paste("&display_coordinates=", displayCoords, sep='')
  if (!is.null(inReplyTo)) {
    if (inherits(inReplyTo, 'status'))
      id <- id(inReplyTo)
    else
      id <- inReplyTo
    url <- paste(url, '&in_reply_to_status_id=', id, sep='')
  }
  out <- buildStatus(doAPICall(url, method="POST"))
}

tweet <- function(text, ...) {
    updateStatus(text, ...)
}

deleteStatus <- function(status, ...) {
  ## FIXME: is 'id' working properly?
  if (!hasOAuth())
    stop("deleteStatus requires OAuth authentication")
  if (!inherits(status, 'status'))
    stop("status argument must be of class status")

  url <- paste("http://api.twitter.com/1/statuses/destroy/",
               id(status), ".json", sep='')
  doAPICall(url, method="POST")
  TRUE
}

publicTimeline <- function(...) {
  jsonList <- doAPICall('http://api.twitter.com/1/statuses/public_timeline.json',
                        ...)
    sapply(jsonList, buildStatus)
}

userTimeline <- function(user, n=20, ...) {
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
                     user, '&count=', n,
                     '&page=', page, sep="")
        jsonList <- c(jsonList, doAPICall(url, ...))
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



showStatus <- function(id, ...) {
  if (!is.numeric(id))
    stop("id argument must be numeric")
  url <- paste("http://api.twitter.com/1/statuses/show/",
               id, ".json", sep="")
  buildStatus(doAPICall(url, ...))
}

userFriends <- function(user, n=100, ...) {
    ffBase(user, 'friends', n, ...)
}

userFollowers <- function(user, n=100, ...) {
    ffBase(user, 'followers', n, ...)
}

ffBase <- function(user, type, n=100, ...) {
    if (inherits(user, 'user'))
        user <- screenName(user)
    if (n <= 0)
        stop("n must be positive")
    n <- as.integer(n)
    baseUrl <- paste('http://api.twitter.com/1/statuses/',
                     type, '/', user, '.json?cursor=', sep='')
    pages <- ceiling(n/100)
    jsonList <- list()
    cursor = -1
    for (i in seq(to=pages)) {
        ## Probably shouldn't be catting multiple lists together,
        ## although this does need to be done sequentially as each
        ## call gets the cursor for the next call
        url <- paste(baseUrl, cursor, sep='')
        json <- doAPICall(url, ...)
        jsonList <- c(jsonList, json$users)
        cursor <- json[['next_cursor']]
    }
    ## Batches come in blocks of 100, so trim to what they requested
    ## if they requested a non-multiple of 100
    if (length(jsonList) > n)
        jsonList <- jsonList[1:n]
    sapply(jsonList, buildUser)
}

