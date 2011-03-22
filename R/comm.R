oauthCache <- new.env(hash=TRUE)

registerOAuth <- function(oauth) {
  if (!inherits(oauth, "OAuth"))
    stop("oauth argument must be of class OAuth")
  if (! oauth$getHandshakeComplete())
    stop("oauth has not completed its handshake")
  assign('oauth', oauth, envir=oauthCache)
  TRUE
}

hasOAuth <- function() {
  exists('oauth', envir=oauthCache)
}

getOAuth <- function() {
  if (!hasOAuth)
    stop("OAuth has not been registered for this session")
  get("oauth", envir=oauthCache)
}

doAPICall <- function(url, curl=getCurlHandle(), method="GET", ...) {
  ## will perform an API call and process the JSON.  In case of failure on 
  ## the latter step, will check to see if HTML was returned (as happens
  ## in some error cases) and if so will attempt up to 3 more times before
  ## returning with an error.  Most twitter HTML errors are very transient in
  ## nature, so this should solve most ills
   
   count <- 1
   while (count < 4) {
     oauth <- try(get("oauth", envir=oauthCache), silent=TRUE)
     ## FIXME:  The OAuthRequest doesn't handle and ... options,
     ##   but not sure how to work that
     if (inherits(oauth, 'try-error'))
       out <- getURL(url, ...)
     else
       out <- oauth$OAuthRequest(url, method, ...)
     if (length(grep('html', out)) == 0) {
       break
     }
     count <- count + 1
   }     
   twFromJSON(out)
}

twFromJSON <- function(json) {
    ## Will provide some basic error checking, as well as suppress
    ## warnings that always seem to come out of fromJSON, even in good cases.
    out <- try(suppressWarnings(fromJSON(json)), silent=TRUE)
    if (inherits(out, "try-error")) {
      stop("Error: Malformed response from server, was not JSON")
    }
    if ('error' %in% names(out)) {
        ## A few errors we want to stop on, and others we want to just
        ## give a warning
        if (length(grep("page parameter out of range",
                        out$error)) > 0) {
            warning("Error: ", out$error)
        } else {
            stop("Error: ", out$error)
        }
    }
    if (length(out) == 2) {
      names <- names(out)
      if ((!is.null(names))&&(all(names(out) == c("request", "error"))))
        stop("Error: ", out$error)
    }
    out
}

twitterDateToPOSIX <- function(dateStr) {
  ## Weird - Date format can vary depending on the situation
  created <- as.POSIXct(dateStr, tz='UTC',
                        format="%a %b %d %H:%M:%S +0000 %Y")
  ## try again if necessary
  if (is.na(created))
    created <- as.POSIXct(dateStr,tz='UTC',
                          format="%a, %d %b %Y %H:%M:%S +0000")
  ## might still be NA, but we tried
  created
}

buildUser <- function(json) {
    status <- buildStatus(json[['status']])

    if ((is.null(json$description))||(is.na(json$description)))
      json$description <- character()
    if (is.null(json$statuses_count))
      json$statuses_count <- numeric()
    if (is.null(json$followers_count))
      json$followers_count <- numeric()
    if (is.null(json$friends_count))
      json$friends_count <- numeric()
    if ((is.null(json$url))||(is.na(json$url)))
      json$url <- character()
    if (is.null(json$name))
      json$name <- character()
    if (is.null(json$created_at)) {
      json$created_at <- Sys.time()
    }
    else {
      json$created_at <- twitterDateToPOSIX(json$created_at)
    }
    if (is.null(json$protected))
        json$protected <- TRUE
    if (is.null(json$verified))
        json$verified <- FALSE
    if (is.null(json$screen_name))
        json$screen_name <- character()
    if ((is.null(json$location))||(is.na(json$location)))
        json$location <- character()
    if (is.null(json$id))
        json$id <- numeric()

    new("user",
        description=json$description,
        statusesCount=json$statuses_count,
        followersCount=json$followers_count,
        friendsCount=json$friends_count,
        url=json$url,
        name=json$name,
        created=json$created_at,
        protected=json$protected,
        verified=json$verified,
        screenName=json$screen_name,
        location=json$location,
        id=json$id,
        lastStatus=status
        )
}

buildStatus <- function(json) {
    if (!is.list(json))
        return(new("status"))

    if ('user' %in% names(json)) {
        user <- buildUser(json[['user']])
        screenName <- screenName(user)
    } else if ('from_user' %in% names(json)) {
        screenName <- json$from_user
    } else {
        screenName <- "Unknown"
    }

    if ((is.null(json$in_reply_to_screen_name))||
        (is.na(json$in_reply_to_screen_name)))
        json$in_reply_to_screen_name <- character()
    if ((is.null(json$in_reply_to_status_id))||
        (is.na(json$in_reply_to_status_id)))
        json$in_reply_to_status_id <- numeric()
    if ((is.null(json$in_reply_to_user_id))||
        (is.na(json$in_reply_to_user_id)))
        json$in_reply_to_user_id <- numeric()

    if (is.null(json$text))
        json$text <- character()
    if (is.null(json$favorited))
        json$favorited <- FALSE
    if (is.null(json$truncated))
        json$truncated <- FALSE
    if (is.null(json$source))
        json$source <- character()
    if (is.null(json$created_at)) {
      json$created_at <- Sys.time()
    } else {
      json$created_at <- twitterDateToPOSIX(json$created_at)
    }
    if (is.null(json$id))
        json$id <- numeric()

    new("status",
        text=json$text,
        favorited=json$favorited,
        replyToSN=json$in_reply_to_screen_name,
        created=json$created_at,
        truncated=json$truncated,
        replyToSID=json$in_reply_to_status_id,
        id=json$id,
        replyToUID=json$in_reply_to_user_id,
        statusSource=json$source,
        screenName=screenName)
}
