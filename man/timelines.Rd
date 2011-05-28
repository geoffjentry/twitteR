\name{timelines}
\alias{publicTimeline}
\alias{userTimeline}
\alias{homeTimeline}
\alias{mentions}
\alias{retweetedByMe}
\alias{retweetedToMe}
\alias{retweetsOfMe}
\title{ Functions to view Twitter timelines }
\description{
  These functions will allow you to retrieve various timelines within
  the Twitter universe
}
\usage{
publicTimeline(...)
userTimeline(user, n=20, maxID=NULL, sinceID=NULL, ...)
homeTimeline(n=25, maxID=NULL, sinceID=NULL, ...)
mentions(n=25, maxID=NULL, sinceID=NULL, ...)
retweetedByMe(n=25, maxID=NULL, sinceID=NULL, ...)
retweetedToMe(n=25, maxID=NULL, sinceID=NULL, ...)
retweetsOfMe(n=25, maxID=NULL, sinceID=NULL, ...)
}
\arguments{
  \item{user}{ The Twitter user to detail, can be \code{character} or
    an \code{\link{user}} object.}
  \item{n}{Number of tweets to retrieve, up to a maximum of 3200}
  \item{maxID}{Maximum ID to search for}
  \item{sinceID}{Minimum (not inclusive) ID to search for}
  \item{...}{Optional arguments to be passed to \code{\link{getURL}}}
}
\details{
  The \code{publicTimeline} function will return a current snapshot of
  the public timeline.

  The \code{userTimeline} function will only work if the user requested
  has a public timeline, or you have previously registered a
  \code{OAuth} object using \code{\link{registerTwitterOAuth}} and are
  authorized to view that content.

  The other functions will provide various views into timelines
  available to the user.  They all require authentication via
  \code{OAuth}.  
}
\value{
  A list of \code{\link{status}} objects
}
\author{ Jeff Gentry }
\seealso{ \code{\link{getUser}}, \code{\link{status}},
  \code{\link{registerTwitterOAuth}}} 
\examples{
        pt <- publicTimeline()
        pt
        ut <- userTimeline('barackobama', n=100)
}
\keyword{ interface }
