\name{rateLimitInfo-class}
\Rdversion{1.1}
\docType{class}
\alias{rateLimitInfo-class}
\alias{rateLimitInfo}
\alias{rateLimitInfoFactory}
\alias{show,rateLimitInfo-method}
\title{Class \code{"rateLimitInfo"}: A class to represent rate limit information}
\description{
This class provides a model representing rate limit information from the Twitter API
}
\section{Fields}{
  \describe{
    \item{\code{remainingHits}:}{Number of remaining API calls before rate limit}
    \item{\code{resetTimeInSeconds}:}{Length of time before the rate limit counter resets}
    \item{\code{hourlyLimt}:}{Number of API calls allowed per hour}
    \item{\code{resetTime}:}{Date of the rate limit reset}
}
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "rateLimitInfo")}: displays the remaining hits for this address }
  }
}
\details{
  The \code{rateLimitInfo} class is implemented as a reference class. The only S4 method
  provided is a \code{show} method.  An instance of a generator for this class is provided as a convenience
  to the user as it is configured to handle most standard cases.  To
  access this generator, use the object \code{rateLimitInfoFactory}.  Accessor set
  & get methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}} for
  more details).  As an example, the \code{hourlyLimit} field could be
  accessed using \code{object$getHourlyLimit()} and \code{object$setHourlyLImit()}.
}
\author{
Jeff Gentry
}
\seealso{
  \code{\link{getCurRateLimitInfo}}, \code{\link{setRefClass}}
}
\examples{
  zz <- getCurRateLimitInfo()
  zz$getRemainingHits()
}
\keyword{classes}

