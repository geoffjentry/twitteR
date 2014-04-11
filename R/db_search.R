#' A function to store searched tweets to a database
#' 
#' A convenience function designed to wrap the process of running a twitter
#' search and pushing the results to a database. If this is called more than
#' once, the search will start with the most recent tweet already stored.
#' 
#' All arguments but \code{table_name} are being passed directly to
#' \code{\link{searchTwitter}}.
#' 
#' This function will check if \code{table_name} exists, and if so will also
#' use a \code{sinceID} of the most recent ID in the table. The search is
#' performed, the returned tweets are stored in the database via
#' \code{\link{store_tweets_db}}.
#' 
#' @param searchString The search string to use, e.g. as one would in
#' \code{\link{searchTwitter}}
#' @param table_name The database to store the tweets to, see
#' \code{\link{register_db_backend}}
#' @param lang If not \code{NULL}, restricts tweets to the given language,
#' given by an ISO 639-1 code
#' @param locale If not \code{NULL}, will set the locale for the search.  As of
#' 03/06/11 only \code{ja} is effective, as per the Twitter API
#' @param geocode If not \code{NULL}, returns tweets by users located within a
#' given radius of the given latitude/longitude. See \code{Details} in
#' \code{link{searchTwitter}}
#' @param retryOnRateLimit If non-zero the search command will block retry up
#' to X times if the rate limit is experienced. This might lead to a much
#' longer run time but the task will eventually complete if the retry count is
#' high enough
#' @param ... Optional arguments to be passed to \code{\link{GET}}
#' @return The number of tweets stored
#' @note Jeff Gentry
#' @seealso \code{\link{register_db_backend}}, \code{\link{searchTwitter}},
#' \code{\link{store_tweets_db}}
#' @keywords utilities
#' @examples
#' 
#'   \dontrun{
#'        register_sqlite_backend("sqlit_file")
#'        n = search_twitter_and_store("#rstats", "rstats_tweets")
#'   }
#' 
search_twitter_and_store = function(searchString, table_name="tweets", lang=NULL, 
                                    locale=NULL, geocode=NULL, retryOnRateLimit=120, ...) {
  if (table_exists(table_name)) {
    since_id = get_latest_tweet_id(table_name)
  } else {
    since_id = NULL
  }
  
  new_tweets = suppressWarnings(searchTwitter(searchString, n=5000, sinceID=since_id, lang=lang, 
                                              locale=locale, retryOnRateLimit=retryOnRateLimit, ...))
  if (length(new_tweets) > 0) {
    store_tweets_db(new_tweets, table_name)
  }
  
  length(new_tweets)
}
