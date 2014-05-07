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
  if (is.null(json)) {
    NULL
  } else {
    statusFactory$new(json)
  }
}

setMethod("show", signature="status", function(object) {
    print(paste(screenName(object), object$text, sep=": "))
})

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

showStatus = function(id, ...) {
  check_id(id)  
  buildStatus(twInterfaceObj$doAPICall(paste('statuses', 'show', id, sep='/'), ...))
}

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

favorites = function(user, n=20, max_id=NULL, since_id=NULL, ...) {
  uParams = parseUsers(user)
  cmd = "favorites/list"
  params = buildCommonArgs(max_id=max_id, since_id=since_id)
  params[["user_id"]] = uParams[["user_id"]]
  params[["screen_name"]] = uParams[["screen_name"]]
  return(statusBase(cmd, params, n, 200, ...))
}

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
