\name{favorites}
\alias{favorites}
\title{
A function to get favorite tweets
}
\description{
Returns the n most recently favorited tweets from the specified user.
}
\usage{
favorites(user, n = 20, max_id = NULL, since_id = NULL, ...)
}
\arguments{
  \item{user}{The Twitter user to detail, can be \code{character} or an \code{\link{user}} object.}
  \item{n}{Number of tweets to retrieve, up to a maximum of 200}
  \item{max_id}{Maximum ID to search for}
  \item{since_id}{Minimum ID to search for}
  \item{\dots}{Optional arguments to pass along to RCurl}
}
\value{
A list of \code{link{status}} objects corresponding to the \code{n} most recent tweets
}
\references{
\url{https://dev.twitter.com/rest/reference/get/favorites/list}
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{getUser}}, \code{\link{status}}
}
\examples{
  \dontrun{
      fav = favorites("barackobama", n=100)
  }
}
\keyword{ interface }
