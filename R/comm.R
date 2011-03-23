oauthCache <- new.env(hash=TRUE)

registerTwitterOAuth <- function(oauth) {
  require("ROAuth") || stop("ROAuth must be installed for ",
                            "OAuth functionality")
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

doAPICall <- function(url, method="GET", ...) {
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
       out <- getURL(URLencode(url), ...)
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


