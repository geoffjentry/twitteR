
getTwitterOAuth = function(consumer_key, consumer_secret) {
  stop("ROAuth is no longer used in favor of httr, please see ?setup_twitter_oauth")
}

registerTwitterOAuth <- function(oauth) {
  stop("ROAuth is no longer used in favor of httr, please see ?setup_twitter_oauth")
}

setup_twitter_oauth = function(consumer_key, consumer_secret, access_token, access_secret,
                               credentials_file=NULL) {
  Sys.setenv(TWITTER_CONSUMER_SECRET=consumer_secret)
  app = suppressMessages(oauth_app("twitter", key=consumer_key))
  sig = suppressMessages(sign_oauth1.0(app, token=access_token, token_secret=access_secret))
  set_oauth_sig(sig)
  if (!is.null(credentials_file)) {
    save(sig, file=credentials_file)
  }
}

load_twitter_oauth = function(credentials_file) {
  if (!file.exists(credentials_file)) {
    stop("credentials_file does not exist!")
  }  
  load(credentials_file)
  set_oauth_sig(sig)
}

set_oauth_sig = function(sig) {
  assign("oauth_sig", sig, envir=oauthCache)  
}

has_oauth_sig = function() {
  exists("oauth_sig", envir=oauthCache)
}

get_oauth_sig = function() {
  if (!has_oauth_sig()) {
    stop("OAuth has not been registered for this session")
  }  
  return(get("oauth_sig", envir=oauthCache))
}

## twitter API has multiple methods of handling paging issues, not to mention the search API
## has a completely different interface.  Trying to manage all of these below using one unified
## approach to actually sending the data back & receiving response and then providing multiple
## mechanisms to page
tw_from_response = function(response) {
  ## Will provide some basic error checking, as well as suppress
  ## warnings that always seem to come out of fromJSON, even
  ## in good cases. 
  out <- try(suppressWarnings(content(response, as="parsed")), silent=TRUE)
  if (inherits(out, "try-error")) {
    stop("Error: Malformed response from server, was not JSON.\n",
         "The most likely cause of this error is Twitter returning a character which\n",
         "can't be properly parsed by R. Generally the only remedy is to wait long\n",
         "enough for the offending character to disappear from searches (e.g. if\n",
         "using searchTwitter()).")
  }
  
  return(out)
}

doAPICall = function(cmd, params=NULL, method="GET", retryCount=5, 
                     retryOnRateLimit=0, ...) {
  if (!is.numeric(retryOnRateLimit)) {
    stop("retryOnRateLimit must be a number")
  }
  
  if (!is.numeric(retryCount)) {
    stop("retryCount must be a number")
  }
  
  recall_func = function(retryCount, rateLimitCount) {
    return(doAPICall(cmd, params=params, method=method, retryCount=retryCount,
                     retryOnRateLimit=rateLimitCount, ...))
  }
  url = getAPIStr(cmd)
  if (method == "POST") {
    out = try(POST(url, get_oauth_sig(), body=params), silent=TRUE)
  } else {
    out = try(GET(url, query=lapply(params, function(x) URLencode(as.character(x))), get_oauth_sig()), silent=TRUE)
  }
  if (inherits(out, "try-error")) {
    error_message = gsub("\\r\\n", "", attr(out, "condition")[["message"]])
    print(error_message)
    if (error_message %in% c("Internal Server Error", "Service Unavailable")) {
      print(paste("This error is likely transient, retrying up to", retryCount, "more times ..."))
      ## These are typically fail whales or similar such things
      Sys.sleep(1)
      return(recall_func(retryCount - 1, rateLimitCount=retryOnRateLimit))         
    } else if (error_message == "Too Many Requests") {
      if (retryOnRateLimit > 0) {
        ## We're rate limited. Wait a while and try again
        print("Rate limited .... blocking for a minute ...")
        Sys.sleep(60)
        return(recall_func(retryCount, retryOnRateLimit - 1))      
      } else {
        ## FIXME: very experimental - the idea is that if we're rate limited,
        ## just give a warning and return. This should result in rate limited
        ## operations returning the partial result
        warning("Rate limit encountered & retry limit reached - returning partial results")
        ## Setting out to {} will have the JSON creator provide an empty list
        out = "{}" 
      }
    } else {
      stop("Error: ", error_message)
    }
  } 
 
  json = tw_from_response(out, ...)

  if (length(json[["errrors"]]) > 0) {
    stop(json[["errors"]][[1]][["message"]])
  }
  
  return(json)  
}

setRefClass('twAPIInterface',
            fields = list(
              maxResults = 'integer'
              ),
            methods = list(
              initialize=function(...) {
                maxResults <<- 100L
                callSuper(...)
                .self
              },
              tw_from_response = tw_from_response,
              doAPICall = doAPICall
              )
            )


tint <- getRefClass('twAPIInterface')
tint$accessors(names(tint$fields()))
twInterfaceObj <- tint$new()


doPagedAPICall = function(cmd, num, params=NULL, method='GET', ...) {
  if (num <= 0)
    stop('num must be positive')
  else
    num <- as.integer(num)

  maxResults <- twInterfaceObj$getMaxResults()
  page <- 1
  total <- num
  count <- ifelse(num < maxResults, num, maxResults)
  jsonList <- list()
  params[['count']] <- count
  while (total > 0) {
    params[['page']] <- page
    jsonList <- c(jsonList,
                  twInterfaceObj$doAPICall(cmd, params, method, ...))
    total <- total - count
    page <- page + 1
  }
  jLen <- length(jsonList)
  if ((jLen > 0) && (jLen > num))
    jsonList <- jsonList[1:num]
  jsonList
}

doCursorAPICall = function(cmd, type, num=NULL, params=NULL, method='GET', ...) {
  cursor <- -1
  if (!is.null(num)) {
    if (num <= 0)
      stop("num must be positive")
    else
      num <- as.integer(num)
  }
  vals <- character()
  while(cursor != 0) {
    params[['cursor']] <- cursor
    curResults <- twInterfaceObj$doAPICall(cmd, params, method, ...)
    vals <- c(vals, curResults[[type]])
    if ((!is.null(num)) && (length(vals) >= num))
      break
    cursor <- curResults[['next_cursor_str']]
  }
  if ((!is.null(num)) && (length(vals) > num))
    vals <- vals[1:num]
  vals
}

doRppAPICall = function(cmd, num, params, ...) {
  if (! 'q' %in% names(params))
    stop("parameter 'q' must be supplied")
  maxResults <- twInterfaceObj$getMaxResults()
  params[['result_type']] <- 'recent'
  params[['count']] <- ifelse(num < maxResults, num, maxResults)
    
  curDiff <- num
  jsonList <- list()
  while (curDiff > 0) {
    fromJSON <- twInterfaceObj$doAPICall(cmd, params, 'GET', ...)
    newList <- fromJSON$statuses
    if (length(newList) == 0) {
      break
    }
    jsonList <- c(jsonList, newList)
    curDiff <- num - length(jsonList)
    if ((curDiff > 0) && (length(newList) == params[["count"]])) {
      params[["max_id"]] = as.character(as.int64(min(sapply(newList, function(x) x$id))) - 1)      
    } else {
      break
    }
  }
  
  if (length(jsonList) > num) {
    jsonList = jsonList[seq_len(num)]
  }
  
  if (length(jsonList) < num) {
    warning(num, " tweets were requested but the API can only return ", length(jsonList))    
  }
  
  return(jsonList)
}

twitterDateToPOSIX <- function(dateStr) {
  ## In typical twitter fashion, there are multiple ways that they
  ## spit dates back at us.  First, let's take a look at unix
  ## epoch time, and then try a few data string formats
  dateInt <- suppressWarnings(as.numeric(dateStr))

  ## Locale must be set to something american-y in order to properly
  ## parse the Twitter dates. Get the current LC_TIME, reset it on
  ## exit and then change the locale
  curLocale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", curLocale), add=TRUE)
  Sys.setlocale("LC_TIME", "C")
  
  if (!is.na(dateInt)) {
    posDate <- as.POSIXct(dateInt, tz='UTC', origin='1970-01-01')
  } else {
    posDate <- as.POSIXct(dateStr, tz='UTC',
                          format="%a %b %d %H:%M:%S +0000 %Y")
    ## try again if necessary
    if (is.na(posDate))
      posDate <- as.POSIXct(dateStr, tz='UTC',
                            format="%a, %d %b %Y %H:%M:%S +0000")
  }
  ## might still be NA, but we tried
  return(posDate)
}


