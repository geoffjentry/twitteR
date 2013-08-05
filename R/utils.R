check_id = function(id) {
  if (!is.character(id)) {
    warning("Using numeric id value can lead to unexpected results for very large ids")
  }
  if (is.na(as.numeric(id))) {
    stop("Malformed id, while it must be a string all ids must be representable as an integer")
  }

  return(TRUE)
}

decode_short_url <- function(url, ...) {
  request_url = paste("http://api.longurl.org/v2/expand?url=", url, "&format=json", sep="")
  return(fromJSON(getURL(request_url, useragent="twitteR", ...))[["long-url"]])
}

getAPIStr <- function(cmd, version=1.1) {
  if (hasOAuth()) {
    scheme <- "https"
  } else {
    scheme <- "http"
  }
  paste(scheme, '://api.twitter.com/', version, '/', cmd, '.json', sep='')
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

twListToDF <- function(twList) {
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

