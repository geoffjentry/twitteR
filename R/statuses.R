setRefClass('statusList',
            contains='twitterObjList'
            )

setValidity('statusList', function(object) {
  listClassValidity(object, 'status')
})

setRefClass("status",
            contains='twitterObj',
            fields = list(
              text="character",
              favorited="logical",
              replyToSN="character",
              created="POSIXct",
              truncated="logical",
              replyToSID="character",
              id="character",
              replyToUID="character",
              statusSource="character",
              screenName="character",
              retweetCount="numeric",
              retweeted="logical",
              longitude="character",
              latitude="character"
              ),
            methods=list(
              initialize = function(json, ...) {
                if (!missing(json)) {
                  if ('user' %in% names(json)) {
                    userObj <- userFactory$new(json[['user']])
                    screenName <<- userObj$getScreenName()
                  } else if ('from_user' %in% names(json)) {
                    screenName <<- json[['from_user']]
                  } else {
                    screenName <<- "Unknown"
                  }
                  if (!is.null(json[['text']]))
                    text <<- json[['text']]
                  if ((is.null(json[['favorited']])) ||
                      (json[["favorited"]] == FALSE)) {
                    favorited <<- FALSE
                  } else {
                    favorited <<- TRUE
                  }
                  if ((is.null(json[['truncated']])) ||
                      (json[["truncated"]] == FALSE)) {
                    truncated <<- FALSE
                  } else {
                    truncated <<- TRUE
                  }
                  if (!is.null(json[['source']]))
                    statusSource <<- json[['source']]
                  if (is.null(json[['created_at']]))
                    created <<- Sys.time()
                  else
                    created <<- twitterDateToPOSIX(json[['created_at']])
                  if ((!is.null(json[['in_reply_to_screen_name']])) &&
                      (!is.na(json[['in_reply_to_screen_name']]))) {
                    replyToSN <<- json[['in_reply_to_screen_name']]
                  }
                  if ((!is.null(json[['in_reply_to_status_id_str']])) &&
                      (!is.na(json[['in_reply_to_status_id_str']]))) {
                    replyToSID <<- as.character(json[['in_reply_to_status_id_str']])
                  }
                  if ((!is.null(json[['in_reply_to_user_id_str']])) &&
                      (!is.na(json[['in_reply_to_user_id_str']]))) {
                    replyToUID <<- as.character(json[['in_reply_to_user_id_str']])
                  }
                  if (!is.null(json[['id_str']])) {
                    id <<- as.character(json[['id_str']])
                  }
                  if (!is.null(json[["retweet_count"]])) {
                    retweetCount <<- as.numeric(json[["retweet_count"]])
                  }
                  if ((is.null(json[['retweeted']])) ||
                      (json[["retweeted"]] == FALSE)) {
                    retweeted <<- FALSE
                  } else {
                    retweeted <<- TRUE
                  }
                  if (!is.null(json[["coordinates"]]) && !(!is.null(json[["coordinates"]][["coordinates"]]))) {
                    longitude <<- json[["coordinates"]][["coordinates"]]
                    latitude <<- json[["coordinates"]][["coordinates"]]
                  }

                }
                callSuper(...)
              }
              )
            )

statusFactory <- getRefClass("status")
statusFactory$accessors(names(statusFactory$fields()))

buildStatus <- function(json) {
  return(statusFactory$new(json))
}

setMethod("show", signature="status", function(object) {
    print(paste(screenName(object), object$text, sep=": "))
})


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
  json <- twInterfaceObj$doAPICall('statuses/update',
                                   params=params, method='POST', ...)
  return(buildStatus(json))
}
            
tweet <- function(text, ...) {
    updateStatus(text, ...)
}

deleteStatus <- function(status, ...) {
  if (!hasOAuth())
    stop("deleteStatus requires OAuth authentication")
  if (!inherits(status, 'status'))
    stop("status argument must be of class status")
  json <- twInterfaceObj$doAPICall(paste('statuses/destroy',
                                         status$getId(), sep='/'),
                                   method='POST', ...)
  if (is.null(json$errors)) {
    TRUE
  } else {
    for (error in json$errors) {
      cat(error$message, error$code, fill = TRUE)
    }
    FALSE
  }
}

showStatus <- function(id, ...) {
  if (!is.numeric(id))
    stop("id argument must be numeric")
  buildStatus(twInterfaceObj$doAPICall(paste('statuses', 'show', id, sep='/'), ...))
}

userTimeline <- function(user, n=20, maxID=NULL, sinceID=NULL, includeRts=FALSE, ...) {
  uParams <- parseUsers(user)
  cmd <- 'statuses/user_timeline'
  params <- buildCommonArgs(max_id=maxID, since_id=sinceID)
  params[['user_id']] <- uParams[['user_id']]
  params[['screen_name']] <- uParams[['screen_name']]
  params[["include_rts"]] <- ifelse(includeRts == TRUE, "true", "false")
  statusBase(cmd, params, n, 3200, ...)
}

homeTimeline <- function(n=25, maxID=NULL, sinceID=NULL, ...) 
  authStatusBase(n, 'home_timeline', maxID=maxID, sinceID=sinceID, ...)

mentions <- function(n=25, maxID=NULL, sinceID=NULL, ...)
  authStatusBase(n, 'mentions_timeline', maxID=maxID, sinceID=sinceID, ...)

retweetsOfMe <- function(n=25, maxID=NULL, sinceID=NULL, ...)
  authStatusBase(n, 'retweets_of_me', maxID=maxID, sinceID=sinceID, ...)

authStatusBase <- function(n, type, maxID=NULL, sinceID=NULL, ...) {
  if (!hasOAuth()) {
    stop("OAuth is required for this functionality")
  }
  
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
  sapply(doPagedAPICall(cmd, n, params, ...), buildStatus)
}
