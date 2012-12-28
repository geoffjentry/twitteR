\name{getCurRateLimitInfo}
\alias{getCurRateLimitInfo}
\title{
  A function to retrieve current rate limit information
}
\description{
  Will retrieve the current rate limit information for the authenticated user, 
  displayed as a data.frame displaying specifc information for every Twitter resource
}
\usage{
getCurRateLimitInfo(resources=character(), ...)
}
\arguments{
  \item{resources}{An optional character vector of specific resources to get information for}
  \item{\dots}{Optional arguments to pass to cURL}
}
\details{
  Using the \code{resources} argument will filter the returned data.frame to only
  list the specified resource values

  The full list of allowed values in \code{resources} is as follows: \code{lists},
  \code{application}, \code{friendships}, \code{blocks}, \code{geo}, \code{users},
  \code{followers}, \code{statuses}, \code{help}, \code{friends}, \code{direct_messages},
  \code{account}, \code{favorites}, \code{saved_searches}, \code{search}, \code{trends}
}
\value{
  A four column data.frame with columns \code{resource}, \code{limit}, \code{remaining}
  and \code{reset}. These detail the specific resource name, the rate limit for that block,
  the number of calls remaining and the time the rate limit will be reset in UTC time.
}
\author{
Jeff Gentry
}
\examples{
  \dontrun{
    zz <- getCurRateLimitInfo(c("lists", "users"))
  }
}
\keyword{ interface }
