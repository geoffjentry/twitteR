Rtweets <- function(n=25, lang=NULL, since=NULL, ...) {
    searchTwitter("#rstats", n=n, lang=lang, since=since, ...)
}



#' Search twitter
#' 
#' This function will issue a search of Twitter based on a supplied search
#' string.
#' 
#' These commands will return any authorized tweets which match the search
#' criteria.  Note that there are pagination restrictions as well as other
#' limits on what can be searched, so it is always possible to not retrieve as
#' many tweets as was requested with the \code{n} argument.  Authorized tweets
#' are public tweets as well as those protected tweets that are available to
#' the user after authenticating via \code{\link{registerTwitterOAuth}}.
#' 
#' For the \code{geocode} argument, the values are given in the format
#' \code{latitude,longitude,radius}, where the radius can have either \code{mi}
#' (miles) or \code{km} (kilometers) as a unit.  For example
#' \code{geocode='37.781157,-122.39720,1mi'}.
#' 
#' For the \code{sinceID} argument, if the requested ID value is older than the
#' oldest available tweets, the API will return tweets starting from the oldest
#' ID available.
#' 
#' The \code{Rtweets} function is a wrapper around \code{searchTwitter} which
#' hardcodes in a search for \code{#rstats}.
#' 
#' @aliases searchTwitter Rtweets searchTwitteR
#' @param searchString Search query to issue to twitter
#' @param n The maximum number of tweets to return
#' @param lang If not \code{NULL}, restricts tweets to the given language,
#' given by an ISO 639-1 code
#' @param since If not \code{NULL}, restricts tweets to those since the given
#' date.  Date is to be formatted as YYYY-MM-DD
#' @param until If not \code{NULL}, restricts tweets to those up until the
#' given date.  Date is to be formatted as YYYY-MM-DD
#' @param locale If not \code{NULL}, will set the locale for the search.  As of
#' 03/06/11 only \code{ja} is effective, as per the Twitter API
#' @param geocode If not \code{NULL}, returns tweets by users located within a
#' given radius of the given latitude/longitude. See \code{Details} below for
#' more information
#' @param sinceID If not \code{NULL}, returns tweets with IDs greater (ie
#' newer) than the specified ID
#' @param retryOnRateLimit If non-zero the search command will block retry up
#' to X times if the rate limit is experienced. This might lead to a much
#' longer run time but the task will eventually complete if the retry count is
#' high enough
#' @param ... Optional arguments to be passed to \code{\link{GET}}
#' @return A list of \code{\link{status}} objects
#' @author Jeff Gentry
#' @seealso \code{\link{status}}
#' @keywords interface
#' @examples
#' 
#'   \dontrun{
#'   	searchTwitter("#beer", n=100)
#'           Rtweets(n=37)
#'   
#'   	## Search between two dates
#'           searchTwitter('charlie sheen', since='2011-03-01', until='2011-03-02')
#'   
#'   	## geocoded results
#'   	searchTwitter('patriots', geocode='42.375,-71.1061111,10mi')
#'   }
#' 
searchTwitter <- function(searchString, n=25, lang=NULL,
                          since=NULL, until=NULL, locale=NULL,
                          geocode=NULL, sinceID=NULL, 
                          retryOnRateLimit=120, ...) {

  
  if (nchar(searchString) > 1000) {
    stop("searchString can only be up to 1000 characters")
  }
  
  if (n <= 0)
        stop("n must be positive")
    n <- as.integer(n)
  
  if (is.null(since)) {
    since_date = NULL
  } else {
    since_date = strsplit(since, " ")[[1]][1]
  }
  if (is.null(until)) {
    until_date = NULL
  } else {
    until_date = strsplit(until, " ")[[1]][1] 
    if (until_date == since_date) {
      ## If since & until are on the same day nothing will be returned. Move
      ## until up a day and then we'll filter this later
      until_date = as.Date(since_date) + 1
    }
  }

  if (!is.null(geocode)) {
    geocheck = strsplit(geocode[[1]], ',')[[1]]
    lat = as.numeric(geocheck[1])
    lon = as.numeric(geocheck[2])
    if ((lon > 180) || (lon < -180)) {
      stop('Longitude need to be in range [180,-180].')
    }
    if ((lat > 90)||(lat < -90)) {
      stop('Latitude need to be in range [90.0,-90.0].')
    }
  }

  params <- buildCommonArgs(lang=lang, locale=locale, since=since_date, until=until_date,
                            geocode=geocode, since_id=sinceID)
  params[['q']] <- searchString
  params[["include_entities"]] = TRUE
  
  jsonList <- doRppAPICall("search/tweets", n, params=params, retryOnRateLimit=retryOnRateLimit, ...)
  statuses = import_statuses(jsonList)
  
  datetimes = sapply(statuses, function(x) x$getCreated())
  if (is.null(since)) {
    since_statuses = seq_along(statuses)
  } else {
    since_statuses = which(datetimes >= as.numeric(as.POSIXct(since, tz="UTC")))
  }
  if (is.null(until)) {
    until_statuses = seq_along(statuses)
  } else {
    until_statuses = which(datetimes <= as.numeric(as.POSIXct(until, tz="UTC")))
  }
  good_statuses = intersect(since_statuses, until_statuses)
  return(statuses[good_statuses])
}

## I don't know how many times I've typod this, making an alias
searchTwitteR = searchTwitter
