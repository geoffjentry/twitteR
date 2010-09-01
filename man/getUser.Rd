\name{getUser}
\alias{getUser}
\alias{userFriends}
\alias{userFollowers}
\alias{statusesBase}
\title{ Functions to manage Twitter users }
\description{
  These functions allow you interact with information about a Twitter
  user - retrieving their base information, list of friends, list of
  followers, and an up to date timeline.
}
\usage{
getUser(user, session = getCurlHandle(), ...)
userFriends(user, n=100, session = getCurlHandle(), ...)
userFollowers(user, n=100, session = getCurlHandle(), ...)
}
\arguments{
  \item{user}{ The Twitter user to detail, can be \code{character} or
    an \code{\link{user}} object.}
  \item{n}{ The maximum number of objects to retrieve }
  \item{session}{ A session object from \code{\link{initSession}} }
  \item{...}{Optional arguments to be passed to \code{\link{getURL}}}
}
\details{
  These functions will only return fully formed objects if the
  authenticated user is allowed to see the requested user.  If that
  person has a private account and has not allowed you to see them, you
  will not be able to extract that information.

  By twiddling the \code{n} parameter you can retrieve more than 100
  objects, as available (setting it to 500 will still only retrieve 300
  if 300 is all that matches the query).  Note that there are pagination
  limits.
}
\value{
  The \code{getUser} function returns an object of class
  \code{\link{user}}.

  The functions \code{userFriends} and \code{userFollowers} return lists
  of class \code{\link{user}} objects.
}
\author{Jeff Gentry}
\seealso{ \code{\link{initSession}}, \code{\link{mentions}}}
\examples{
      tuser <- getUser('geoffjentry')
      tfriends <- userFriends(tuser)
      tfollowers <- userFollowers(tuser)
      ttimeline <- userTimeline(tuser)
}
\keyword{ interface }
