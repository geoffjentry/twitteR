setRefClass('statusList',
            contains='twitterObjList'
            )

setValidity('statusList', function(object) {
  listClassValidity(object, 'status')
})

#' Class to contain a Twitter status
#' 
#' Container for Twitter status messages, including the text as well as basic
#' information
#' 
#' The \code{status} class is implemented as a reference class.  This class was
#' previously implemented as an S4 class, and for backward compatibility
#' purposes the old S4 accessor methods have been left in, although new code
#' should not be written with these.  An instance of a generator for this class
#' is provided as a convenience to the user as it is configured to handle most
#' standard cases.  To access this generator, use the object
#' \code{statusFactory}.  Accessor set & get methods are provided for every
#' field using reference class \code{$accessors()} methodology (see
#' \code{\link{setRefClass}} for more details).  As an example, the
#' \code{screenName} field could be accessed using \code{object$getScreenName}
#' and \code{object$setScreenName}.
#' 
#' The constructor of this object assumes that the user is passing in a JSON
#' encoded Twitter status.  It is also possible to directly pass in the
#' arguments.
#' 
#' @name status-class
#' @aliases status-class statusFactory status buildStatus show,status-method
#' as.data.frame,status-method text,status-method favorited,status-method
#' favorited replyToSN,status-method replyToSN created,status-method
#' truncated,status-method truncated replyToSID,status-method replyToSID
#' id,status-method id replyToUID,status-method replyToUID
#' statusSource,status-method statusSource screenName,status-method statusText
#' statusText,status-method retweetCount,status-method retweetCount
#' retweeted,status-method retweeted [[,twitterObjList-method
#' as.data.frame,twitterObj-method show,twitterObjList-method
#' @docType class
#' @section Fields: \describe{ \item{list("text")}{The text of the
#' status}\item{:}{The text of the status} \item{list("screenName")}{Screen
#' name of the user who posted this status}\item{:}{Screen name of the user who
#' posted this status} \item{list("id")}{ID of this status}\item{:}{ID of this
#' status} \item{list("replyToSN")}{Screen name of the user this is in reply
#' to}\item{:}{Screen name of the user this is in reply to}
#' \item{list("replyToUID")}{ID of the user this was in reply to}\item{:}{ID of
#' the user this was in reply to} \item{list("statusSource")}{Source user agent
#' for this tweet}\item{:}{Source user agent for this tweet}
#' \item{list("created")}{When this status was created}\item{:}{When this
#' status was created} \item{list("truncated")}{Whether this status was
#' truncated}\item{:}{Whether this status was truncated}
#' \item{list("favorited")}{Whether this status has been
#' favorited}\item{:}{Whether this status has been favorited}
#' \item{list("retweeted")}{TRUE if this status has been
#' retweeted}\item{:}{TRUE if this status has been retweeted}
#' \item{list("retweetCount")}{The number of times this status has been
#' retweeted}\item{:}{The number of times this status has been retweeted} }
#' @author Jeff Gentry
#' @seealso \code{\link{userTimeline}}, \code{\link{setRefClass}}
#' @keywords classes
#' @examples
#' 
#'    \dontrun{
#'      st <- statusFactory$new(screenName="test", text="test message")
#'      st$getScreenName()
#'      st$getText()
#' 
#'      ## Assume 'json' is the return from a Twitter call
#'      st <- statusFactory$new(json)
#'      st$getScreenName()
#'    }
#'    
#'
setRefClass("status",
            contains='twitterObj',
            fields = list(
              text="character",
              favorited="logical",
              favoriteCount="numeric",
              replyToSN="character",
              created="POSIXct",
              truncated="logical",
              replyToSID="character",
              id="character",
              replyToUID="character",
              statusSource="character",
              screenName="character",
              retweetCount="numeric",
              isRetweet="logical",
              retweeted="logical",
              longitude="character",
              latitude="character",
              urls="data.frame"
              ),
            methods=list(
              initialize = function(json, ...) {
                if (!missing(json)) {
                  if ('user' %in% names(json)) {
                    userObj <- userFactory$new(json[['user']])
                    screenName <<- userObj$getScreenName()
                  } else if ('from_user' %in% names(json)) {
                    screenName <<- json[['from_user']]
                  } else if ("screen_name" %in% names(json)) {
                    screenName <<- json[["screen_name"]]  
                  }  else {
                    screenName <<- "Unknown"
                  }
                  
                  if (!is.null(json[['text']])) {
                    text <<- json[['text']]
                  }
                  
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
                  
                  status_source = get_json_value(json, c("source", "status_source"))
                  if (!is.null(status_source)) {
                    statusSource <<- status_source
                  }

                  created_at = get_json_value(json, c("created_at", "created"))
                  if (is.null(created_at)) {
                    created <<- Sys.time()
                  } else {
                    created <<- twitterDateToPOSIX(created_at)
                  }
                  
                  in_reply_to_screen_name = get_json_value(json, c("reply_to_s_n", "in_reply_to_screen_name"))
                  if (!is.null(in_reply_to_screen_name) && (!is.na(in_reply_to_screen_name))) {
                    replyToSN <<- as.character(in_reply_to_screen_name)
                  }
                  
                  in_reply_to_sid = get_json_value(json, c("reply_to_s_i_d", "in_reply_to_status_id_str"))
                  if ((!is.null(in_reply_to_sid)) && (!is.na(in_reply_to_sid))) {
                    replyToSID <<- as.character(in_reply_to_sid)
                  }

                  reply_to_uid = get_json_value(json, c("reply_to_u_i_d", "in_reply_to_user_id_str"))
                  if ((!is.null(reply_to_uid)) && (!is.na(reply_to_uid))) {
                    replyToUID <<- as.character(reply_to_uid)
                  }

                  # Note: Make sure id_str is first here, otherwise numeric id will be snagged
                  id_field = get_json_value(json, c("id_str", "id"))
                  if (!is.null(id_field)) {
                    id <<- as.character(id_field)
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
                  if (!is.null(json[["favorite_count"]])) {
                    favoriteCount <<- as.numeric(json[["favorite_count"]])
                  }
                  if (!is.null(json[["coordinates"]]) && (!is.null(json[["coordinates"]][["coordinates"]]))) {
                    longitude <<- as.character(json[["coordinates"]][["coordinates"]][1])
                    latitude <<- as.character(json[["coordinates"]][["coordinates"]][2])
                  } else {
                    if (!is.null(json[["longitude"]])) {
                      longitude <<- as.character(json[["longitude"]])
                    }
                    if (!is.null(json[["latitude"]])) {
                      latitude <<- as.character(json[["latitude"]])
                    }
                  }
                  
                  ## If retweeted_status is provided (which contains the full original status), this is a retweet
                  isRetweet <<- any(c("retweeted_status", "isRetweet") %in% names(json))
                  
                  urls <<- build_urls_data_frame(json)
                }
                callSuper(...)
              },
              getRetweets = function(n=20, ...) {
                return(retweets(self$getId(), n, ...))
              },
              getRetweeters = function(n=20, ...) {
                return(retweeters(self$getId(), n, ...))
              },
              toDataFrame = function(row.names=NULL, optional=FALSE, stringsAsFactors=FALSE) {
                callSuper(row.names=row.names, optional=optional, stringsAsFactors=stringsAsFactors, 
                          fieldsToRemove="urls")
              }
              )
            )


statusFactory = getRefClass("status")
statusFactory$accessors(names(statusFactory$fields()))

buildStatus = function(json) {
  return(statusFactory$new(json))
}

setMethod("show", signature="status", function(object) {
    print(paste(screenName(object), object$text, sep=": "))
})



#' Functions to manipulate Twitter status
#' 
#' These functions can be used to set or delete a user's Twitter status
#' 
#' These messages will only operate properly if the user is authenticated via
#' \code{OAuth}
#' 
#' The \code{tweet} and \code{updateStatus} functions are the same.
#' 
#' To delete a status message, pass in an object of class \code{\link{status}},
#' such as from the return value of \code{updateStatus}.
#' 
#' @aliases updateStatus deleteStatus tweet
#' @param text The text to use for a new status
#' @param status An object of class \code{\link{status}}
#' @param lat If not \code{NULL}, the latitude the status refers to.  Ignored
#' if no \code{long} parameter is provideded
#' @param long If not \code{NULL}, the longitude the status refers to.  Ignored
#' if no \code{lat} parameter is provideded
#' @param placeID If not \code{NULL}, provideds a place in the world.  See
#' Twitter documentation for details
#' @param displayCoords Whether or not to put a pin on the exact coordinates a
#' tweet has been sent from, \code{true} or \code{false} if not \code{NULL}
#' @param inReplyTo If not \code{NULL}, denotes the status this is in reply to.
#' Either an object of class \code{\link{status}} or an ID value
#' @param mediaPath If not \code{NULL}, file path to a supported media format
#' (PNG, JPG and GIF) to be included in the status update
#' @param ... Optional arguments to be passed to \code{\link{GET}}
#' @return The \code{updateStatus} function will return an object of class
#' \code{\link{status}}.
#' 
#' The \code{deleteStatus} returns \code{TRUE} on success and an error if
#' failure occurs.
#' @author Jeff Gentry
#' @keywords interface
#' @examples
#' 
#'    \dontrun{
#'       ns <- updateStatus('this is my new status message')
#'       ## ooops, we want to remove it!
#'       deleteStatus(ns)
#'    }
#' 
updateStatus <- function(text, lat=NULL, long=NULL, placeID=NULL,
                         displayCoords=NULL, inReplyTo=NULL, mediaPath=NULL, ...) {
  if (!has_oauth_token())
    stop("updateStatus requires OAuth authentication")

  if (nchar(text) > 140)
    stop("Status can not be more than 140 characters")

  params = buildCommonArgs(lat=lat, long=long, place_id=placeID,
                            display_coordinates=displayCoords,
                            in_reply_to_status_id=inReplyTo)
  params[['status']] <- text

  if (is.null(mediaPath)){
	endpoint = 'statuses/update'
  } else {
  	endpoint = 'statuses/update_with_media'
    params[['media']] <- upload_file(mediaPath)
  }
  json = twInterfaceObj$doAPICall(endpoint,
                                 params=params, method='POST', ...)
  return(buildStatus(json))
}
            
tweet = function(text, ...) {
    return(updateStatus(text, ...))
}

deleteStatus = function(status, ...) {
  if (!has_oauth_token()) {
    stop("deleteStatus requires OAuth authentication")
  }
  if (!inherits(status, 'status')) {
    stop("status argument must be of class status")
  }
  
  json = twInterfaceObj$doAPICall(paste('statuses/destroy',
                                       status$getId(), sep='/'),
                                       method='POST', ...)
  if (is.null(json$errors)) {
    return(TRUE)
  } else {
    for (error in json$errors) {
      cat(error$message, error$code, fill = TRUE)
    }
    return(FALSE)
  }
}



#' A function to return one specific tweet
#' 
#' This function will take a numeric ID of a tweet and return it to the user
#' 
#' 
#' @param id Numerical ID of a specific tweet
#' @param \dots Optional arguments to be passed to \code{\link{GET}}
#' @return An object of class \code{\link{status}}
#' @author Jeff Gentry
#' @seealso \code{\link{status}}
#' @keywords interface
#' @examples
#' 
#'  \dontrun{
#'     showStatus('123')
#'  }
#' 
showStatus = function(id, ...) {
  check_id(id)  
  buildStatus(twInterfaceObj$doAPICall(paste('statuses', 'show', id, sep='/'), ...))
}



#' Functions to work with retweets
#' 
#' These functions can be used to return retweets or users who retweeted a
#' tweet
#' 
#' 
#' @aliases retweets retweeters
#' @param id The ID of the tweet to get retweet information on
#' @param n The number of results to return, up to 100
#' @param \dots Further arguments to pass on to httr
#' @return For \code{retweets} the n most recent retweets of the original
#' tweet.
#' 
#' For \code{retweeters} the n most recent users who have retweeted this tweet.
#' @author Jeff Gentry
#' @seealso \code{\link{showStatus}}
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'   \dontrun{
#'      retweets("21947795900469248")
#'      
#'      st = showStatus("21947795900469248")
#'      retweeters(st$getId())
#'   }
#' 
retweets = function(id, n=20, ...) {
  check_id(id)

  if (n > 100) {
    stop("n must be less than 100, set to ", n)
  }
  
  cmd = "statuses/retweets"
  params = list(id=id, count=n)
  return(sapply(doAPICall(cmd, params=params), buildStatus))  
}

retweeters = function(id, n=20, ...) {
  check_id(id)
  
  cmd = "statuses/retweeters/ids"
  params = list(id=id, count=n)
  json = doCursorAPICall(cmd, "ids", num=n, params=params, method="GET", ...)
  json
}



#' A function to get favorite tweets
#' 
#' Returns the n most recently favorited tweets from the specified user.
#' 
#' 
#' @param user The Twitter user to detail, can be \code{character} or an
#' \code{\link{user}} object.
#' @param n Number of tweets to retrieve, up to a maximum of 200
#' @param max_id Maximum ID to search for
#' @param since_id Minimum ID to search for
#' @param \dots Optional arguments to pass along to RCurl
#' @return A list of \code{link{status}} objects corresponding to the \code{n}
#' most recent tweets
#' @author Jeff Gentry
#' @seealso \code{\link{getUser}}, \code{\link{status}}
#' @references \url{https://dev.twitter.com/docs/api/1.1/get/favorites/list}
#' @keywords interface
#' @examples
#' 
#'   \dontrun{
#'       fav = favorites("barackobama", n=100)
#'   }
#' 
favorites = function(user, n=20, max_id=NULL, since_id=NULL, ...) {
  uParams = parseUsers(user)
  cmd = "favorites/list"
  params = buildCommonArgs(max_id=max_id, since_id=since_id)
  params[["user_id"]] = uParams[["user_id"]]
  params[["screen_name"]] = uParams[["screen_name"]]
  return(statusBase(cmd, params, n, 200, ...))
}

#' Functions to view Twitter timelines
#' 
#' These functions will allow you to retrieve various timelines within the
#' Twitter universe
#' 
#' 
#' @aliases userTimeline homeTimeline mentions retweetsOfMe
#' @param user The Twitter user to detail, can be \code{character} or an
#' \code{\link{user}} object.
#' @param n Number of tweets to retrieve, up to a maximum of 3200
#' @param maxID Maximum ID to search for
#' @param sinceID Minimum (not inclusive) ID to search for
#' @param includeRts If \code{FALSE} any native retweets (not old style RT
#' retweets) will be stripped from the results
#' @param excludeReplies if \code{TRUE} any replies are stripped from the
#' results
#' @param ... Optional arguments to be passed to \code{\link{GET}}
#' @return A list of \code{\link{status}} objects
#' @author Jeff Gentry
#' @seealso \code{\link{getUser}}, \code{\link{status}}
#' @keywords interface
#' @examples
#' 
#'   \dontrun{
#'         ut <- userTimeline('barackobama', n=100)
#'   }
#' 
userTimeline = function(user, n=20, maxID=NULL, sinceID=NULL, includeRts=FALSE, excludeReplies=FALSE, ...) {
  uParams <- parseUsers(user)
  cmd <- 'statuses/user_timeline'
  params <- buildCommonArgs(max_id=maxID, since_id=sinceID)
  params[['user_id']] <- uParams[['user_id']]
  params[['screen_name']] <- uParams[['screen_name']]
  params[["include_rts"]] <- ifelse(includeRts == TRUE, "true", "false")
  params[["exclude_replies"]] <- ifelse(excludeReplies == TRUE, "true", "false")
  return(statusBase(cmd, params, n, 3200, ...))
}

homeTimeline <- function(n=25, maxID=NULL, sinceID=NULL, ...) {
  return(authStatusBase(n, 'home_timeline', maxID=maxID, sinceID=sinceID, ...))
}

mentions <- function(n=25, maxID=NULL, sinceID=NULL, ...) {
  return(authStatusBase(n, 'mentions_timeline', maxID=maxID, sinceID=sinceID, ...))
}

retweetsOfMe <- function(n=25, maxID=NULL, sinceID=NULL, ...) {
  return(authStatusBase(n, 'retweets_of_me', maxID=maxID, sinceID=sinceID, ...))
}

authStatusBase <- function(n, type, maxID=NULL, sinceID=NULL, ...) {
  if (!has_oauth_token()) {
    stop("OAuth is required for this functionality")
  }
  
  params <- buildCommonArgs(max_id=maxID, since_id=sinceID)
  cmd <- paste('statuses', type, sep='/')
  cmd <- paste('statuses', type, sep='/')
  return(statusBase(cmd, params, n, 800, ...))
}

statusBase <- function(cmd, params, n, maxN, ...) {
  n <- as.integer(n)
  if (n > maxN) {
    warning(cmd, " has a cap of ", maxN, " statuses, clipping")
    n <- maxN
  }
  return(sapply(doPagedAPICall(cmd, n, params, ...), buildStatus))
}

build_urls_data_frame = function(json) {
  ## takes a status JSON and will either return a data.frame of the URLs entity or an
  ## empty data.frame if there were none provided
  split_indices = function(urls_block) {
    urls_block$start_index = urls_block$indices[1]
    urls_block$stop_index = urls_block$indices[2]
    urls_block$indices = NULL
    urls_block
  }
  
  if (length(json$entities$urls) > 0) {
    urls = json$entities$urls
    massaged_urls = lapply(urls, split_indices)
    return(do.call("rbind", lapply(massaged_urls, as.data.frame, stringsAsFactors=FALSE)))
  } else {
    data.frame(url=character(), expanded_url=character(), dispaly_url=character(), indices=numeric(), stringsAsFactors=FALSE)
  }
}
