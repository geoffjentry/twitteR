updateStatus <- function(text, lat=NULL, long=NULL, placeID=NULL,
                         displayCoords=NULL, inReplyTo=NULL, ...) {
  if (!hasOAuth())
    stop("updateStatus requires OAuth authentication")

  if (nchar(text) > 140)
    stop("Status can not be more than 140 characters")

  params <- buildCommonArgs(lat=lat, long=long, place_id=placeID,
                            display_coordinates=displayCoords,
                            in_reply_to_status_id=inReplyTo)
  params[['status']] <- text
  buildStatus(twInterfaceObj$doAPICall('statuses/update',
                                       params=params, method='POST', ...))
}
            
tweet <- function(text, ...) {
    updateStatus(text, ...)
}

deleteStatus <- function(status, ...) {
  if (!hasOAuth())
    stop("deleteStatus requires OAuth authentication")
  if (!inherits(status, 'status'))
    stop("status argument must be of class status")
  twInterfaceObj$doAPICall('statuses/destroy', params=list(id=status$getId()), method='POST', ...)
  TRUE
}

publicTimeline <- function(...) {
  jsonList <- twInterfaceObj$doAPICall('statuses/public_timeline')
  sapply(jsonList, buildStatus)
}

showStatus <- function(id, ...) {
  if (!is.numeric(id))
    stop("id argument must be numeric")
  buildStatus(twInterfaceObj$doAPICall(paste('statuses', 'show', id, sep='/'), ...))
}

userTimeline <- function(user, n=20, maxID=NULL, sinceID=NULL, ...) {
    ## AUTH: Will not work if user is protected until OAuth
    if (inherits(user, "user"))
        user <- user$getScreenName()
    cmd <- 'statuses/user_timeline'
    params <- buildCommonArgs(max_id=maxID, since_id=sinceID)
    numUser <- suppressWarnings(as.numeric(user))
    if (is.na(numUser))
      params[['screen_name']] <- user
    else
      params[['user_id']] <- numUser
    statusBase(cmd, params, n, 3200, ...)
}

homeTimeline <- function(n=25, maxID=NULL, sinceID=NULL, ...) 
  authStatusBase(n, 'home_timeline', maxID=maxID, sinceID=sinceID, ...)

mentions <- function(n=25, maxID=NULL, sinceID=NULL, ...)
  authStatusBase(n, 'mentions', maxID=maxID, sinceID=sinceID, ...)

retweetedByMe <- function(n=25, maxID=NULL, sinceID=NULL, ...)
  authStatusBase(n, 'retweeted_by_me', maxID=maxID, sinceID=sinceID, ...)

retweetedToMe <- function(n=25, maxID=NULL, sinceID=NULL, ...) 
  authStatusBase(n, 'retweeted_to_me', maxID=maxID, sinceID=sinceID, ...)

retweetsOfMe <- function(n=25, maxID=NULL, sinceID=NULL, ...)
  authStatusBase(n, 'retweets_of_me', maxID=maxID, sinceID=sinceID, ...)

authStatusBase <- function(n, type, maxID=NULL, sinceID=NULL, ...) {
  if (!hasOAuth())
    stop("OAuth is required for this functionality")

  params <- buildCommonArgs(max_id=maxID, since_id=sinceID)
  cmd <- paste('statuses', type, sep='/')
  cmd <- paste('statuses', type, sep='/')
  statusBase(cmd, params, n, 800, ...)
}

statusBase <- function(cmd, params, n, maxN, ...) {
  n <- as.integer(n)
  if (n > maxN) {
    warning(cmd, " has a cap of ", maxN, " statuses, clipping")
    n <- maxN
  }
  sapply(twInterfaceObj$doPagedAPICall(cmd, n, params, ...), buildStatus)
}
