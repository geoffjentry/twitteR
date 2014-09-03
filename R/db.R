

store_tweets_db = function(tweets, table_name="tweets") {
  if (inherits(tweets, "status")) {
    tweets = list(tweets)
  }
  check_list_classes(tweets, "status")
  store_db(tweets, table_name)
}

load_tweets_db = function(as.data.frame=FALSE, table_name="tweets") {
  tweet_df = load_from_db(table_name)
  
  if (as.data.frame) {
    if (length(setdiff(colnames(df), tweet_columns)) > 0) {
      stop("Malformed tweet data.frame, columns don't match")
    }
    
    # Some column types need to be massaged from the DB if we're going straight to DF
    for (logical_col in c("favorited", "truncated", "isRetweet", "retweeted")) {
      tweet_df[, logical_col] = convert_logical_column(tweet_df[, logical_col])
    }
    tweet_df$created = convert_date_column(tweet_df$created)
    
    tweet_df
  } else {
    # We don't need to do the checks & type massaging as the normal structure
    # of the import will take care of it
    import_statuses(tweet_df, df_to_statuses)
  }
}

store_users_db = function(users, table_name="users") {
  if (inherits(users, "user")) {
    users = list(users)
  }
  check_list_classes(users, "user")
  store_db(users, table_name)
}

load_users_db = function(as.data.frame=FALSE, table_name="users") {
  users_df = load_from_db(table_name)
  
  if (as.data.frame) {
    if (length(setdiff(colnames(df), user_columns)) > 0) {
      stop("Malformed user data.frame, columns don't match")
    }
    
    for (logical_col in c("protected", "verified", "followRequestSent")) {
      users_df[, logical_col] = convert_logical_column(users_df[, logical_col])
    }
    users_df$created = convert_date_column(users_df$created)
    
    users_df
  } else {
    # We don't need to do the checks & type massaging as the normal structure
    # of the import will take care of it
    import_users(users_df, df_to_users)    
  }
}

store_db = function(to_store, table_name) {
  db_handle = get_db_backend()
  df = twListToDF(to_store)
  df = escape_character_columns(df, db_handle)
  dbWriteTable(conn=db_handle, name=table_name, value=df, append=TRUE, overwrite=FALSE,
               row.names=FALSE)  
}

load_from_db = function(table_name) {
  db_handle = get_db_handle_check_table(table_name)
  dbReadTable(db_handle, table_name)
}

get_db_handle_check_table = function(table_name) {    
  if (!table_exists(table_name)) {
    stop("Supplied table_name does not exist: ", table_name)
  }

  get_db_backend()
}

table_exists = function(table_name) {
  db_handle = get_db_backend()
  table_name %in% dbListTables(db_handle)  
}

convert_logical_column = function(column) {
  column == 1
}

escape_character_columns = function(df, db_handle) {
  escape_character_column = function(column) {
    nas = which(is.na(column))
    new_column = RMySQL::dbEscapeStrings(db_handle, column)
    new_column[nas] = NA
    new_column
  }
  
  # Some DBI types have issues with stuff like \n in character fields,
  # strip these out if dbEscapeStrings exists
  if (attr(class(db_handle), "package") == "RMySQL") {
    char_columns = which(sapply(df, class) == "character")
    for (column in char_columns) {
      df[, column] = escape_character_column(df[, column])
    }
  }
  
  df
}

convert_date_column = function(column) {
  as.POSIXct(column, tz="UTC", origin="1970-01-01")
}

check_list_classes = function(to_store, required_class) {
  if (!all(sapply(to_store, class) == required_class)) {
    stop("Invalid input, all entities in list must be of class ", required_class)
  }
}