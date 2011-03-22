\name{publicTimeline}
\alias{publicTimeline}
\alias{userTimeline}
\title{ Functions to view Twitter timelines }
\description{
  These functions will allow you to retrieve various timelines within
  the Twitter universe
}
\usage{
publicTimeline(session=getCurlHandle(), ...)
userTimeline(user, n=20, ...)
}
\arguments{
  \item{user}{ The Twitter user to detail, can be \code{character} or
    an \code{\link{user}} object.}
  \item{n}{Number of tweets to retrieve, up to a maximum of 3200}
  \item{...}{Optional arguments to be passed to \code{\link{getURL}}}
}
\details{
  The \code{userTimeline} function will only work if the user requested
  has a public timeline, or you have previously registered a
  \code{\link{OAuth}} object using \code{\link{registerOAuth}} and are
  authorized to view that content.
}
\value{
  A list of \code{\link{status}} objects
}
\author{ Jeff Gentry }
\seealso{ \code{\link{getUser}}, \code{\link{status}}}
\examples{
        pt <- publicTimeline()
        pt
        ut <- userTimeline('barackobama', n=100)
}
\keyword{ interface }
