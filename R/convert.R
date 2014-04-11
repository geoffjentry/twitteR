import_users = function(raw_data, conversion_func=json_to_users) {
  return(import_obj(raw_data, conversion_func))
}

#' Functions to import twitteR objects from various sources
#' 
#' Functions designed to import data into twitteR objects from a variety of
#' data sources. Currently only JSON is supported, and this entire branch of
#' functionality should be considered experimental & under development.
#' 
#' 
#' @aliases import_statuses import_obj import_trends import_users
#' json_to_statuses json_to_trends json_to_users
#' @param raw_data Data to be be parsed via the prescribed function
#' @param conversion_func The function to convert \code{raw_data} into the
#' specified twitteR object
#' @param ... Arguments to pass along to \code{conversion_func}
#' @return A list of twitteR objects of the appropriate type, e.g.
#' \code{\link{status}}, \code{\link{user}}, etc
#' @author Jeff Gentry
#' @seealso \code{\link{status}}, \code{\link{user}}
#' @keywords interface
#' @examples
#' 
#'    \dontrun{
#'       status_list = import_statuses(list_of_status_json)   
#'    }
#' 
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
