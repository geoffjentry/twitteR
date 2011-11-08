getAPIStr <- function(cmd, version=1) {
  paste('http://api.twitter.com/', version, '/', cmd, '.json', sep='')
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
      out[[arg]] <- val
  }
  out
}

parseUsers <- function(users) {
  ## many functions have 'user' input which can be one of class 'user', a UID or a screen name,
  ## try to do something rational here
  users <- sapply(users, function(x) {
    if (inherits(x, 'user'))
      x$getScreenName()
    else
      x
  })
  numUsers <- suppressWarnings(as.numeric(users))
  uids <- numUsers[!is.na(numUsers)]
  sns <- setdiff(users, uids)
  list(user_id=paste(uids, collapse=','),
       screen_name=paste(sns, collapse=','))
}

