\name{publicTimeline}
\alias{publicTimeline}
\alias{friendsTimeline}
\alias{userTimeline}
\alias{mentions}
\title{ Functions to view Twitter timelines }
\description{
  These functions will allow you to retrieve various timelines within
  the Twitter universe
}
\usage{
publicTimeline(session=getCurlHandle(), ...)
userTimeline(user, session=getCurlHandle(), ...)
friendsTimeline(session=getCurlHandle(), ...)
mentions(session=getCurlHandle(), ...)
}
\arguments{
  \item{user}{ The Twitter user to detail, can be \code{character} or
    an \code{\link{user}} object.}
  \item{session}{ A session object from \code{\link{initSession}} }
  \item{...}{Optional arguments to be passed to \code{\link{getURL}}}
}
\details{
  The \code{userTimeline} function will only properly operate if the
  authenticated user is allowed to see the requested user.

  The \code{mentions} function will return any statuses which mention
  the authenticated user.

  The \code{friendsTImeline} function will return the current timeline
  of the friends of the authenticated user.
}
\value{
  A list of \code{\link{status}} objects
}
\author{ Jeff Gentry }
\seealso{ \code{\link{getUser}}, \code{\link{initSession}}, \code{\link{status}}}
\examples{
    \dontrun{
        sess <- initSession('myUser', 'myPasss')
        pt <- publicTimeline()
        pt
        mentions(sess)
        tw <- getUser('twitter', sess)
        userTimeline(tw, sess)
   }
}
\keyword{ interface }
