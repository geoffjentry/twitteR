\name{showStatus}
\alias{showStatus}
\title{
A function to return one specific tweet
}
\description{
This function will take a numeric ID of a tweet and return it to the user
}
\usage{
showStatus(id, ...)
}
\arguments{
  \item{id}{
    Numerical ID of a specific tweet
}
  \item{session}{
    cURL session object
}
  \item{\dots}{
    Optional arguments to be passed to \code{\link{getURL}}
}
}
\value{
  An object of class \code{\link{status}}
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{status}}
}
\examples{
 \dontrun{
    showStatus('123')
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }

