\name{showStatus}
\alias{showStatus}
\alias{lookup_statuses}
\title{
Functions to return statuses
}
\description{
These functions can be used to retrieve specific tweets from the server
}
\usage{
showStatus(id, ...)
lookup_statuses(ids, ...)
}
\arguments{
  \item{id}{ID of a specific tweet, should be a String, but numbers are accepted}
  \item{ids}{A vector of IDs to lookup, should be Strings but numbers are accepted}
  \item{\dots}{Optional arguments to be passed to \code{\link{GET}} (or \code{\link{POST}}, see Details)}
}
\value{
  For showStatus, an object of class \code{\link{status}}
  
  For lookup_statuses, a list of \code{\link{status}} objects. Note that these will not be in the
  same order as the \code{ids} argument and that any id which could not be retrieved will not be present.
}
\details{
  Ideally a POST request would be used for lookup_statuses, however currently there is a problem
  (issue 78 on github) and GET is used.
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
    lookup_statuses(c("123", "234", "456"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }

