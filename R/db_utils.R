get_latest_tweet_id = function(table_name = "tweets") {
  db_handle = get_db_handle_check_table(table_name)
  max_id = dbGetQuery(db_handle, paste("select max(id) from", table_name))
  if (nrow(max_id) == 0) {
    stop("No existing tweets in ", table_name)
  } else {
    max_id[1, 1]
  }
}

