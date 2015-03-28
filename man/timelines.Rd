\name{timelines}
\alias{userTimeline}
\alias{homeTimeline}
\alias{mentions}
\alias{retweetsOfMe}
\title{ Functions to view Twitter timelines }
\description{
  These functions will allow you to retrieve various timelines within
  the Twitter universe
}
\usage{
userTimeline(user, n=20, maxID=NULL, sinceID=NULL, includeRts=FALSE, 
  excludeReplies=FALSE, ...)
homeTimeline(n=25, maxID=NULL, sinceID=NULL, ...)
mentions(n=25, maxID=NULL, sinceID=NULL, ...)
retweetsOfMe(n=25, maxID=NULL, sinceID=NULL, ...)
listTimeline(user, list_name, n=20, maxID=NULL, sinceID=NULL, includeRts=FALSE, 
  excludeReplies=FALSE, ...)
}
\arguments{
  \item{user}{ The Twitter user to detail, can be \code{character} or
    an \code{\link{user}} object.}
  \item{n}{Number of tweets to retrieve, up to a maximum of 3200}
  \item{maxID}{Maximum ID to search for}
  \item{sinceID}{Minimum (not inclusive) ID to search for}
  \item{includeRts}{If \code{FALSE} any native retweets (not old style RT retweets)
  		       will be stripped from the results} 
  \item{excludeReplies}{if \code{TRUE} any replies are stripped from the results}
  \item{list_name}{ Target list name, used for \code{listTimeline}}
  \item{...}{Optional arguments to be passed to \code{\link{GET}}}
}
\value{
  A list of \code{\link{status}} objects
}
\author{ Jeff Gentry }
\seealso{ \code{\link{getUser}}, \code{\link{status}}} 
\examples{
  \dontrun{
        ut <- userTimeline('barackobama', n=100)
  }
}
\keyword{ interface }
