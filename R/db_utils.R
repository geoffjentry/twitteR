#' A function to retrieve the most recent tweet ID from a database
#' 
#' Given a registered database backend which contains a table of tweets, will
#' return the ID of the most recent tweet stored in that table
#' 
#' A wrapper around a \code{select max(id)} on the \code{table_name}
#' 
#' @param table_name The name of the table in the database containing tweets
#' @return The ID of the most recent tweet in the table, or a
#' \code{\link{stop}} if the table is empty
#' @author Jeff Gentry
#' @seealso \code{\link{register_db_backend}}
#' @keywords utilities
#' @examples
#' 
#'    \dontrun{
#'       register_sqlite_backend("sqlit_file")
#'       get_latest_tweet_id("rstats_tweets")
#'    }
#' 
get_latest_tweet_id = function(table_name = "tweets") {
  db_handle = get_db_handle_check_table(table_name)
  max_id = dbGetQuery(db_handle, paste("select max(id) from", table_name))
  if (nrow(max_id) == 0) {
    stop("No existing tweets in ", table_name)
  } else {
    max_id[1, 1]
  }
}

