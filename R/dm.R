dmGet <- function(n=25, sinceID=NULL, maxID=NULL, ...) {
  dmGETBase(n, sinceID, maxID)
}

dmSent <- function(n=25, sinceID=NULL, maxID=NULL, ...) {
  dmGETBase(n, sinceID, maxID, "/sent")
}

dmGETBase <- function(n, sinceID, maxID, type='', ...) {
  if (!hasOAuth())
    stop("dmGet requires OAuth authentication")

  if (n <= 0)
    stop("n must be positive")
  else
    n <- as.integer(n)
  params <- buildCommonArgs(since_id=sinceID, max_id=maxID)
  jsonList <- twInterfaceObj$doPagedAPICall(paste('direct_messages',
                                                  type, sep=''),
                                            num=n, params=params,
                                       ...)
  sapply(jsonList, function(x) dmFactory$new(x))
}

dmDestroy <- function(dm, ...) {
  if (!hasOAuth())
    stop("dmDestroy requires OAuth authentication")
  if (!inherits(dm, "directMessage"))
    stop("dm must be of class directMessage")
  twInterfaceObj$doAPICall(paste('direct_messages/destroy',
                                 dm$getId(), sep='/'),
                           method='POST', ...)
  TRUE
}

dmSend <- function(text, user, ...) {
  if (!hasOAuth())
    stop("dmSend requires OAuth authentication")
  if (inherits(user, "user"))
        user <- screenName(user)
  if (nchar(text) > 140)
    stop("Maximum of 140 chars may be sent via a direct message")

  params <- list(text=text)
  if (is.numeric(user))
    params[['user_id']] <- user
  else
    params[['screen_name']] <- user
  res <- twInterfaceObj$doAPICall('direct_messages/new',
                                  params=params, method='POST', ...)
  dmFactory$new(res)
}
