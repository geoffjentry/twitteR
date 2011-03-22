\name{updateStatus}
\Rdversion{1.1}
\alias{updateStatus}
\alias{deleteStatus}
\alias{showStatus}
\alias{tweet}
\title{
  Functions to manipulate Twitter status
}
\description{
  These functions can be used to set or delete a user's Twitter status
}
\usage{
tweet(text, ...)
updateStatus(text, ...)
deleteStatus(status, ...)
}
\arguments{
  \item{text}{The text to use for a new status }
  \item{status}{An object of class \code{\link{status}}}
  \item{...}{Optional arguments to be passed to \code{\link{getURL}}}
}
\details{
  These messages will only operate properly if the user is authenticated
  via \code{OAuth}
  
  The \code{tweet} and \code{updateStatus} functions are the same.
   
  To delete a status message, pass in an object of class
  \code{\link{status}}, such as from the return value of \code{updateStatus}.
}
\value{
  The \code{updateStatus} function will return an object of class
  \code{\link{status}}.

  The \code{deleteStatus} returns \code{TRUE} on success and an error if
  failure occurs.
}
\author{
   Jeff Gentry
}
\seealso{
  \code{\link{registerTwitterOAuth}}
}
\examples{
   \dontrun{
      ns <- updateStatus('this is my new status message', sess)
      ## ooops, we want to remove it!
      deleteStatus(ns)
   }
}
\keyword{ interface }
