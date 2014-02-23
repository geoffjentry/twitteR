import_users = function(raw_data, conversion_func=json_to_users) {
  return(import_obj(raw_data, conversion_func))
}

import_statuses = function(raw_data, conversion_func=json_to_statuses) {
  return(import_obj(raw_data, conversion_func))
}

import_trends = function(raw_data, conversion_func=json_to_trends) {
  return(import_obj(raw_data, conversion_func))
}

import_obj = function(raw_data, conversion_func, ...) {
  return(conversion_func(raw_data, ...))
}

json_to_users = function(raw_data) {
  return(sapply(raw_data, buildUser))
}

json_to_statuses = function(raw_data) {
  return(sapply(raw_data, buildStatus))
}

json_to_trends = function(raw_data) {
  buildTrendLocationDf(raw_data)
}

df_to_statuses = function(df) {
  df_to_class(df, tweet_columns, buildStatus)
}

df_to_users = function(df) {
  df_to_class(df, user_columns, buildUser)
}

df_to_class = function(df, columns, builder, colname_converter=camel_case_to_twitter_names) {
  if (length(setdiff(colnames(df), columns)) > 0) {
    stop("Malformed tweet data.frame, columns don't match")
  }
  
  out = vector(mode="list", length=nrow(df))  
  colnames(df) = sapply(colnames(df), colname_converter)  
  for (row in seq_along(df[, 1])) {
    out[[row]] = builder(as.list(df[row, ]))
  }
  
  out
}