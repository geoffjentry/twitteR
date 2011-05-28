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
  if (!hasOAuth())
    stop("OAuth has not been registered for this session")
  get("oauth", envir=oauthCache)
}

setRefClass('twInterface',
            contains = 'VIRTUAL',
            fields = list(
              maxResults = 'integer'
              ),
            methods = list(
              initialize=function(...) {
                .self$maxResults <- 100
                callSuper(...)
                .self
              },
              twFromJSON = function(json) {
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
              },
              doAPICall = function(url, params=NULL, method="GET", ...) {
                ## will perform an API call and process the JSON.  For GET
                ## calls, try to detect errors and if so attempt up to 3
                ## more times before returning with an error.  Many twitter
                ## HTML errors are very transient in nature and if it's a
                ## real error there's little harm in repeating the call.
                ## Don't do this on POST calls in case we incorrectly detect
                ## an error, to avoid pushing the request multiple times.
                if (hasOAuth()) {
                  APIFunc <- function(url, params, method, ...) {
                    oauth <- getOAuth()
                    oauth$OAuthRequest(url, params, method, ...)
                  }
                } else {
                  APIFunc <- function(url, params, method, ...) {
                    if (!is.null(params)) {
                      paramStr <- paste(paste(names(params), params, sep='='),
                                        collapse='&')
                      url <- paste(url, paramStr, sep='?')
                    }
                    getURL(URLencode(url), ...)
                  }
                }
                if (method == "POST") {
                  out <- APIFunc(url, params, method, ...)
                } else {
                  count <- 1
                  while (count < 4) {
                    out <- APIFunc(url, params, method, ...)
                    if (length(grep('html', out)) == 0) {
                      break
                    }
                    count <- count + 1
                  }
                }
                .self$twFromJSON(out)
              }
              )
            )

setRefClass('twAPIInterface',
            contains='twInterface',
            methods = list(
              initialize = function(...) {
                callSuper(...)
                .self
              },
              doAPICall = function(cmd, params=NULL, method='GET', ...) {
                url <- getAPIStr(cmd)
                callSuper(url, params, method, ...)
              }, 
              doPagedAPICall = function(cmd, num, params=NULL,
                method='GET', ...) {
                if (num <= 0)
                  stop('num must be positive')
                else
                  num <- as.integer(num)
                
                page <- 1
                total <- num
                count <- ifelse(num < .self$maxResults, num, .self$maxResults)
                jsonList <- list()
                params[['count']] <- count
                while (total > 0) {
                  params[['page']] <- page
                  jsonList <- c(jsonList,
                                .self$doAPICall(cmd, params, method, ...))
                  total <- total - count
                  page <- page + 1
                }
                jLen <- length(jsonList)
                if ((jLen > 0) && (jLen > n))
                  jsonList <- jsonList[1:num]
                jsonList
              }
              )
            )

twInterfaceObj <- getRefClass('twAPIInterface')$new()


setRefClass('twSearchInterface',
            contains='twInterface',
            methods = list(
              initialize = function(...) {
                callSuper(...)
                .self
              },
              doAPICall = function(num, params, ...) {
                if (! 'q' %in% names(params))
                  stop("parameter 'q' must be supplied")
                params[['result_type']] <- 'recent'
                params[['rpp']] <- ifelse(num < .self$maxResults, num, .self$maxResults)
                params[['page']] <- 1
                
                url <- 'http://search.twitter.com/search.json'

                curDiff <- num
                jsonList <- list()
                while (curDiff > 0) {
                  fromJSON <- callSuper(url, params, 'GET', ...)
                  newList <- fromJSON$results
                  jsonList <- c(jsonList, newList)
                  curDiff <- num - length(jsonList)
                  if (curDiff > 0) {
                    if ('next_page' %in% names(fromJSON)) {
                      ## The search API gives back the params part as an actual URL string, split this
                      ## back into list structure
                      splitParams <- strsplit(strsplit(gsub('\\?', '', URLdecode(newParams)), '&')[[1]], '=')
                      params <- lapply(splitParams, function(x) x[2])
                      names(params) <- sapply(splitParams, function(x) x[1])
                      if (curDiff < .self$maxResults)
                        ## If we no longer want max entities, only get curDiff
                        params[['rpp']] <- curDiff
                    }
                  } else {
                    break
                  }
                }
                jsonList
              }
              )
            )

twSearchInterfaceObj <- getRefClass('twSearchInterface')$new()

twitterDateToPOSIX <- function(dateStr) {
  ## Weird - Date format can vary depending on the situation
  created <- as.POSIXct(dateStr, tz='UTC',
                        format="%a %b %d %H:%M:%S +0000 %Y")
  ## try again if necessary
  if (is.na(created))
    created <- as.POSIXct(dateStr, tz='UTC',
                          format="%a, %d %b %Y %H:%M:%S +0000")
  ## might still be NA, but we tried
  created
}


