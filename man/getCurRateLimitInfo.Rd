\name{getCurRateLimitInfo}
\alias{getCurRateLimitInfo}
\title{
  A function to retrieve current rate limit information
}
\description{
  Will retrieve the current rate limit information.  If the user is authenticated via
  OAuth, will retrieve information for the user account, otherwise it will do it based on
  the IP address
}
\usage{
getCurRateLimitInfo(...)
}
\arguments{
  \item{\dots}{Optional arguments to pass to cURL}
}
\value{
  An object of class \code{\link{rateLimitInfo}}
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{rateLimitInfo}}
}
\examples{
  zz <- getCurRateLimitInfo()
  zz$getResetTimeInSeconds()
}
\keyword{ interface }
