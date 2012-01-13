\name{getUser}
\alias{getUser}
\alias{lookupUsers}
\title{ Functions to manage Twitter users }
\description{
  These functions allow you interact with information about a Twitter
  user - retrieving their base information, list of friends, list of
  followers, and an up to date timeline.
}
\usage{
getUser(user, ...)
lookupUsers(users, includeNA=FALSE, ...)
}
\arguments{
  \item{user}{ The Twitter user to detail, can be \code{character} or
    an \code{\link{user}} object.}
  \item{users}{A vector of either user IDs or screen names or a mix of both}
  \item{includeNA}{If \code{TRUE} will leave an NA element in the return list for users that don't exist}
  \item{...}{Optional arguments to be passed to \code{\link{getURL}}}
}
\details{
  These functions will only return fully formed objects if the
  authenticated user is allowed to see the requested user.  If that
  person has a private account and has not allowed you to see them, you
  will not be able to extract that information.

  The \code{lookupUsers} function should be used in cases where there are
  multiple lookups going to take place, to reduce the API call load.  This function
  requires OAuth authentication.
}
\value{
  The \code{getUser} function returns an object of class
  \code{\link{user}}.

  The \code{lookupUsers} function will return a list of \code{\link{user}} objects,
  sorted in the order of the \code{users} argument, with names being the particular
  element of \code{users} that it matches to. If the \code{includeNA} argument is set
  to \code{FALSE (default), any non-existing users will be dropped from the list.
}
\author{Jeff Gentry}
\seealso{\code{\link{mentions}}}
\examples{
      tuser <- getUser('geoffjentry')
      \dontrun{
        ## This requires OAuth authentication
        users <- lookupUsers(c('geoffjentry', 'whitehouse'))
      }
}
\keyword{ interface }
