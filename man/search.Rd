\name{searchTwitter}
\alias{searchTwitter}
\title{ Search twitter }
\description{
  This function will issue a search of Twitter based on a supplied
  search string.
}
\usage{
searchTwitter(searchString, n=25, session = getCurlHandle(), ...)
}
\arguments{
  \item{searchString}{Search query to issue to twitter}
  \item{session}{ A session object from \code{\link{initSession}}}
  \item{n}{The number of tweets to return}
  \item{...}{Optional arguments to be passed to \code{\link{getURL}}}
}
\details{
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
	searchTwitter("#rstats", n=100)
}
\keyword{ interface }
