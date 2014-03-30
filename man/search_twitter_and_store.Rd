\name{search_twitter_and_store}
\alias{search_twitter_and_store}
\title{
A function to store searched tweets to a database
}
\description{
A convenience function designed to wrap the process of running a twitter search and pushing the results to a database.
If this is called more than once, the search will start with the most recent tweet already stored.
}
\usage{
search_twitter_and_store(searchString, table_name = "tweets", lang = NULL, locale = NULL, geocode = NULL, retryOnRateLimit = 120, ...)
}
\arguments{
  \item{searchString}{The search string to use, e.g. as one would in \code{\link{searchTwitter}}}
  \item{table_name}{The database to store the tweets to, see \code{\link{register_db_backend}}}
  \item{lang}{If not \code{NULL}, restricts tweets to the given
    language, given by an ISO 639-1 code} 
  \item{locale}{If not \code{NULL}, will set the locale for the search.
    As of 03/06/11 only \code{ja} is effective, as per the Twitter API}
  \item{geocode}{If not \code{NULL}, returns tweets by users located
    within a given radius of the given latitude/longitude. See
    \code{Details} in \code{link{searchTwitter}}}
  \item{retryOnRateLimit}{If non-zero the search command will block retry up to
    X times if the rate limit is experienced. This might lead to a much longer run
    time but the task will eventually complete if the retry count is high enough}
  \item{...}{Optional arguments to be passed to \code{\link{GET}}}
}
\details{
All arguments but \code{table_name} are being passed directly to \code{\link{searchTwitter}}.

This function will check if \code{table_name} exists, and if so will also use a \code{sinceID} of the 
most recent ID in the table. The search is performed, the returned tweets are stored
in the database via \code{\link{store_tweets_db}}.
}
\value{
The number of tweets stored
}
\note{Jeff Gentry}
\seealso{
\code{\link{register_db_backend}}, \code{\link{searchTwitter}}, \code{\link{store_tweets_db}}
}
\examples{
  \dontrun{
       register_sqlite_backend("sqlit_file")
       n = search_twitter_and_store("#rstats", "rstats_tweets")
  }
}
\keyword{ utilities }