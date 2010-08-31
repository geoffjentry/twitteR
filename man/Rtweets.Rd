\name{Rtweets}
\alias{Rtweets}
\title{ Retrieve latest R related tweets }
\description{
  This function will look at the \code{#rstats} hashtag and return status
  updates that are related to R
}
\usage{
Rtweets(session = getCurlHandle(), n=25, ...)
}
\arguments{
  \item{session}{ A session object from \code{\link{initSession}}}
  \item{n}{The number of tweets to return}
  \item{...}{Items to be passed to \code{\link{getURL}}}
}
\details{
  This is not a full featured search, but simply a hard coded search for
  the \code{#rstats} tag.

  A premade session is typically not going to be necessary, this command
  can be called without any parameters.  However, if a user has access
  to a protected user's timeline, they will not see any \code{#rstat} activity
  without using a \code{session} object.
}
\value{
  A list of \code{\link{status}} objects
}
\author{Jeff Gentry}
\seealso{ \code{\link{status}} }
\examples{
           Rtweets()
}
\keyword{ interface }
