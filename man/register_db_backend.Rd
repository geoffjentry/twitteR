\name{register_db_backend}
\alias{register_db_backend}
\alias{register_sqlite_backend}
\alias{register_mysql_backend}
\title{
Functions to setup a database backend for twitteR
}
\description{
twitteR can have a database backend registered from which to store and load
tweet and user data. These functions provide mechanisms for setting up the 
connection within twitteR
}
\usage{
register_db_backend(db_handle)
register_sqlite_backend(sqlite_file, ...)
register_mysql_backend(db_name, host, user, password, ...)
}
\arguments{
  \item{db_handle}{A DBI connection}
  \item{sqlite_file}{File path for a SQLite file}
  \item{db_name}{Name of the database to connect to}
  \item{host}{Hostname the database is on}
  \item{user}{username to connect to the database with}
  \item{password}{password to connect to the database with}
  \item{...}{extra arguments to pass to \code{dbConnect}}
}
\details{
  Currently only \code{RSQLite} and \code{RMySQL} are supported. To use either
  of these DBI implementations the appropriate packages will need to be 
  installed.
  
  The \code{register_sqlite_backend} and \code{register_mysql_backend} are 
  convenience wrappers to both create the DBI connection and call \code{register_db_backend}
  for you.
}
\value{
  The DBI connection, invisibly
}
\author{Jeff Gentry}
\seealso{
\code{\link{store_tweets_db}}, \code{\link{store_users_db}}, \code{\link{load_tweets_db}}, \code{\link{load_users_db}}
}
\examples{
   \dontrun{
    register_sqlite_backend("/path/to/sqlite/file")
    tweets = searchTwitter("#scala")
    store_tweets_db(tweets)
    from_db = load_tweets_db()
   }
}
\keyword{ utilities }