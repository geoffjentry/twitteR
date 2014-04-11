require_package = function(package_name) {
  if (!require(package_name, character.only=TRUE, quietly=TRUE)) {
    stop(package_name, " is required to be installed for this activity")
  }
}

supported_db_handle_classes = c("SQLiteConnection", "MySQLConnection")

# Based on the class of db_handle, make sure the appropriate dbi backend
# package exists
require_dbi_package = function(db_handle) {
  if (!inherits(db_handle, supported_db_handle_classes)) {
    stop("db_handle must be one of the following classes: ", 
         paste(supported_db_handle_classes, collapse=", "))
  }
  
  switch(class(db_handle),
         SQLiteConnection = require_package("RSQLite"),
         MySQLConnection = require_package("RMySQL")
         )
  
  TRUE
}

# Convenience function which will wrap some of the DBI commands to get a SQLiteConnection
register_sqlite_backend = function(sqlite_file, ...) {
  if (!require("RSQLite")) {
    stop("RSQLite package must be installed to generate an SQLite handle")
  }
  
  register_db_backend(dbConnect(dbDriver("SQLite"), sqlite_file, ...))
}

# Convenience function which will wrap the DBI command for a MySQL connection
register_mysql_backend = function(db_name, host, user, password, ...) {
  if (!require("RMySQL")) {
    stop("RMySQL package must be installed to generate a MySQL handle")
  }
  
  db_handle = register_db_backend(dbConnect(dbDriver("MySQL"), user=user, password=password, 
                                  dbname=db_name, host=host, ...))
  dbGetQuery(db_handle, "SET NAMES 'utf8'")
  
  invisible(db_handle)
}

#' Functions to setup a database backend for twitteR
#' 
#' twitteR can have a database backend registered from which to store and load
#' tweet and user data. These functions provide mechanisms for setting up the
#' connection within twitteR
#' 
#' Currently only \code{RSQLite} and \code{RMySQL} are supported. To use either
#' of these DBI implementations the appropriate packages will need to be
#' installed.
#' 
#' The \code{register_sqlite_backend} and \code{register_mysql_backend} are
#' convenience wrappers to both create the DBI connection and call
#' \code{register_db_backend} for you.
#' 
#' @aliases register_db_backend register_sqlite_backend register_mysql_backend
#' @param db_handle A DBI connection
#' @param sqlite_file File path for a SQLite file
#' @param db_name Name of the database to connect to
#' @param host Hostname the database is on
#' @param user username to connect to the database with
#' @param password password to connect to the database with
#' @param ... extra arguments to pass to \code{dbConnect}
#' @return The DBI connection, invisibly
#' @author Jeff Gentry
#' @seealso \code{\link{store_tweets_db}}, \code{\link{store_users_db}},
#' \code{\link{load_tweets_db}}, \code{\link{load_users_db}}
#' @keywords utilities
#' @examples
#' 
#'    \dontrun{
#'     register_sqlite_backend("/path/to/sqlite/file")
#'     tweets = searchTwitter("#scala")
#'     store_tweets_db(tweets)
#'     from_db = load_tweets_db()
#'    }
#' 
register_db_backend = function(db_handle) {
  require_dbi_package(db_handle)
  assign("db_handle", db_handle, envir=db_backend_cache)  
}

has_db_backend = function() {
  exists("db_handle", envir=db_backend_cache)
}

get_db_backend = function() {
  if (!has_db_backend()) {
    stop("No DB backend has been registered, see ?register_db_backend")
  }
  
  get("db_handle", envir=db_backend_cache)
}
