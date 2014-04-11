get_json_value = function(json, names) {
  for (name in names) {
    if (!is.null(json[[name]])) {
      return(json[[name]])
    }
  }
  
  NULL
}

check_id = function(id) {
  if (!is.character(id)) {
    warning("Using numeric id value can lead to unexpected results for very large ids")
  }
  if (is.na(as.numeric(id))) {
    stop("Malformed id, while it must be a string all ids must be representable as an integer")
  }

  return(TRUE)
}



#' A function to decode shortened URLs
#' 
#' Will expand a URL that has been processed by a link shortener (e.g. bit.ly).
#' Provided as a convenience function to users who may which to perform this
#' operation.
#' 
#' Uses the \url{longapi.org} API
#' 
#' @param url A character string, the URL to decode
#' @param \dots Optional arguments to pass along to RCurl
#' @return A character string containing either the original URL (if not
#' shortened) or the full URL (if shortened)
#' @author Neil Jang
#' @references \url{longapi.org}
#' @keywords utilities
#' @examples
#' 
#'   \dontrun{
#'     decode_short_url("http://bit.ly/23226se656")
#'   }
#' 
decode_short_url <- function(url, ...) {
  request_url = paste("http://api.longurl.org/v2/expand?url=", url, "&format=json", sep="")
  response = GET(request_url, query=list(useragent="twitteR"), ...)
  parsed = content(response, as="parsed")
  if (! "long-url" %in% names(parsed)) {
    return(url)
  } else {
    return(parsed[["long-url"]])
  }
}

getAPIStr <- function(cmd, version=1.1) {
  paste("https://api.twitter.com/", version, '/', cmd, '.json', sep='')
}

buildCommonArgs <- function(lang=NULL, since=NULL, until=NULL, locale=NULL,
                            geocode=NULL, since_id=NULL, max_id=NULL,
                            lat=NULL, long=NULL, place_id=NULL,
                            display_coordinates=NULL,
                            in_reply_to_status_id=NULL, exclude=NULL,
                            date=NULL) {
  out <- list()
  for (arg in names(formals())) {
    val <- get(arg)
    if (length(val) > 0)
      out[[arg]] <- as.character(val)
  }
  out
}

parseUsers <- function(users) {
  ## many functions have 'user' input which can be one of class 'user', a UID or a screen name,
  ## try to do something rational here
  if ((length(users) == 1) && (inherits(users, "user"))) {
    users = users$getScreenName()
  } else {
    users <- sapply(users, function(x) {
      if (inherits(x, 'user'))
        x$getScreenName()
      else
        x
    })
  }
  
  numUsers <- suppressWarnings(as.numeric(users))
  uids <- numUsers[!is.na(numUsers)]
  screen.names <- setdiff(users, uids)
  
  return(buildUserList(uids, screen.names))
}

buildUserList = function(uids, screen_names) {
  user_list = list()  
  if (length(uids) > 0) {
    user_list$user_id = paste(uids, collapse=',')
  }
  if (length(screen_names) > 0) {
    user_list$screen_name = paste(screen_names, collapse=',')
  }
  
  return(user_list)  
}



#' A function to convert twitteR lists to data.frames
#' 
#' This function will take a list of objects from a single twitteR class and
#' return a data.frame version of the members
#' 
#' The classes supported by this function are \code{\link{status}},
#' \code{\link{user}}, and \code{\link{directMessage}}.
#' 
#' @param twList A list of objects of a single twitteR class, restrictions are
#' listed in \code{details}
#' @return A \code{\link{data.frame}} with rows corresponding to the objects in
#' the list and columns being the fields of the class
#' @author Jeff Gentry
#' @seealso \code{\link{status}}, \code{\link{user}},
#' \code{\link{directMessage}}
#' @keywords interface
#' @examples
#' 
#'   \dontrun{
#'     zz <- searchTwitter("#rstats")
#'     twListToDF(zz)
#'   }
#' 
twListToDF <- function(twList) {
  if (length(twList) == 0) {
    stop("Empty list passed to twListToDF")
  }
  
  ## iff all elements of twList are from a class defined in this
  ## package, and all of the same class, will collapse these into
  ## a data.frame and return
  classes <- unique(sapply(twList, class))
  if (length(classes) != 1) {
    stop("Not all elements of twList are of the same class")
  }
  if (! classes %in% c('status', 'user', 'directMessage')) {
    stop("Elements of twList are not of an appropriate class")
  }
  do.call('rbind', lapply(twList, as.data.frame))
}



#' A function to remove retweets
#' 
#' Given a list of status objects, will remove retweets from the list to
#' provide a "pure" set of tweets.
#' 
#' Newer style retweets are summarily removed regardless of options.
#' 
#' Older style retweets (aka manual retweets) are tweets of the form \code{RT
#' @user blah blah}. If \code{strip_manual} is \code{TRUE}, tweets containing
#' the \code{RT} string will have everything including and to the right of the
#' \code{RT} will be removed. Everything to the left of the \code{RT} will
#' remain, as this should be original content.
#' 
#' If \code{strip_mt} is \code{TRUE}, tweets will be stripped in the same
#' manner as \code{strip_manual} but using the string \code{MT}
#' 
#' @param tweets A list of \code{\link{status}} objects
#' @param strip_manual If \code{TRUE} will remove old style manual retweets
#' @param strip_mt If \code{TRUE} will remove modified tweets (MT)
#' @return A list of \code{status} objects with retweeted content removed
#' @author Jeff Gentry
#' @seealso \code{\link{status}}
#' @keywords utilities
#' @examples
#' 
#'   \dontrun{
#'     tweets = searchTwitter("stuff")
#'     no_retweets = strip_retweets(tweets)
#'   }
#' 
strip_retweets = function(tweets, strip_manual=TRUE, strip_mt=TRUE) {
  if ((!is.list(tweets)) || (!all(sapply(tweets, function(x) inherits(x, "status"))))) {
    stop("tweets argument must be a list of class status")
  }
  ## Find/remove the tweets flagged as retweets
  is_retweets = which(sapply(tweets, function(x) x$getIsRetweet()))
  
  if (length(is_retweets) > 0) {
    filtered_tweets = tweets[-is_retweets]
  } else {
    filtered_tweets = tweets
  }
  
  if (strip_manual) {
    statuses = sapply(filtered_tweets, function(x) x$getText())
    
    if (strip_mt) {
      rt_pattern = "(RT|MT)"
    } else {
      rt_pattern = "RT"
    }
    
    ## Find and remove RT based retweets. This will be overeager but we're not losing many
    ## tweets anyways
    split_tweets = sapply(strsplit(statuses, paste0("[[:space:]]?", rt_pattern)), function(x) x[1])
    manual_retweets = which(split_tweets == "")
    if (length(manual_retweets) > 0) {
      filtered_tweets = filtered_tweets[-manual_retweets]
    }
  }
  
  filtered_tweets
}
