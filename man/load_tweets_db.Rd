\name{load_tweets_db}
\alias{load_tweets_db}
\alias{load_users_db}
\alias{store_tweets_db}
\alias{store_users_db}
\title{
  Functions to persist/load twitteR data to a database
}
\description{
  These functions allow a user to store twitteR based data to a database backend
  as well as retrieving previously stored data
}
\usage{
store_tweets_db(tweets, table_name="tweets")
store_users_db(users, table_name="users")
load_users_db(as.data.frame = FALSE, table_name = "users")
load_tweets_db(as.data.frame = FALSE, table_name = "tweets")
}
\arguments{
  \item{tweets}{A list of \code{status} objects to persist to the database}
  \item{users}{A list of \code{user} objects to persist to the database}
  \item{as.data.frame}{if \code{TRUE}, data will be returned as a data.frame instead of twitteR objects}
  \item{table_name}{The database table to use for storing and loading}
}
\value{
  \code{store_tweets_db} and \code{store_users_db} return \code{TRUE} of \code{FALSE}
  based on their success or not. The loading functions return either a \code{data.frame}
  of the data (representing the underlying table) or a list of the appropriate \code{twitteR}
  objects.
}
\author{Jeff Gentry}
\seealso{
\code{\link{register_db_backend}}, \code{\link{register_sqlite_backend}}, \code{\link{register_mysql_backend}}
}
\examples{
   \dontrun{
    register_sqlite_backend("/path/to/sqlite/file")
    tweets = searchTwitter("#scala")
    store_tweets_db(tweets)
    from_db = load_tweets_db()
   }
}
\keyword{ utilities}