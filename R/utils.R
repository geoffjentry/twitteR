getAPIStr <- function(cmd, version=1) {
  paste('http://api.twitter.com/', version, '/', cmd, '.json', sep='')
}

buildCommonArgs <- function(lang=NULL, since=NULL, until=NULL, locale=NULL, geocode=NULL,
                            since_id=NULL, max_id=NULL, lat=NULL, long=NULL,
                            place_id=NULL, display_coordinates=NULL, in_reply_to_status_id=NULL) {
  out <- list()
  for (arg in names(formals)) {
    val <- get(arg)
    if (length(val) > 0)
      out[[arg]] <- val
  }
  out
}

taskStatus <- function(expr, to, msg="") {
  if (!hasOAuth())
    stop("taskStatus requires OAuth authentication")
  
  status <- try(expr, silent=TRUE)
    if (inherits(status, "try-error")) {
        out <- paste(paste("Your task failed with error message",
                           status), msg, ":")
        dmSend(out, to)
    }
    else {
        out <- paste("Your task has completed successfully",
                     msg, sep=":")
        dmSend(out, to)
    }
    status
}

