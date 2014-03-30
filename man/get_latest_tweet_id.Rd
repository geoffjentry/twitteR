\name{get_latest_tweet_id}
\alias{get_latest_tweet_id}
\title{
A function to retrieve the most recent tweet ID from a database
}
\description{
Given a registered database backend which contains a table of tweets, will return the
ID of the most recent tweet stored in that table
}
\usage{
get_latest_tweet_id(table_name = "tweets")
}
\arguments{
  \item{table_name}{The name of the table in the database containing tweets}
}
\details{
A wrapper around a \code{select max(id)} on the \code{table_name}
}
\value{
The ID of the most recent tweet in the table, or a \code{\link{stop}} if the table is empty
}
\author{Jeff Gentry}
\seealso{\code{\link{register_db_backend}}}
\examples{
   \dontrun{
      register_sqlite_backend("sqlit_file")
      get_latest_tweet_id("rstats_tweets")
   }
}
\keyword{utilities}