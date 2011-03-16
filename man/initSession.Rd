\name{initSession}
\alias{initSession}
\alias{twFromJSON}
\title{ A function to initiate a Twitter session }
\description{
  This function will initiate a web session for the user with the
  Twitter servers, providing a handle which is then used throughout the
  rest of the functions of \code{twitteR}.
}
\usage{
initSession(username, password)
}
\arguments{
  \item{username}{ Your Twitter screenname }
  \item{password}{ Your Twitter password }
}
\details{
  As of now, \code{twitteR} still uses the old authentication method,
  and not \code{OAuth}.  This function provides a \code{CURLHandle}
  which can then be passed along to any other function within the
  package in order to achieve proper authentication.
}
\value{
  An object of class \code{CURLHandle}
}
\author{Jeff Gentry}
\seealso{\code{getCurlHandle}, \code{\link{dmGet}}, \code{\link{getUser}}}
\examples{
   \dontrun{
       sess <- initSession('myscreen', 'mypass')
   }
}
\keyword{ interface }
