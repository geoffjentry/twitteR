\name{updateStatus}
\Rdversion{1.1}
\alias{updateStatus}
\alias{deleteStatus}
\alias{tweet}
\title{
  Functions to manipulate Twitter status
}
\description{
  These functions can be used to set or delete a user's Twitter status
}
\usage{
tweet(text, ...)
updateStatus(text, lat=NULL, long=NULL, placeID=NULL,
             displayCoords=NULL, inReplyTo=NULL, mediaPath=NULL, 
             bypassCharLimit=FALSE, ...)
deleteStatus(status, ...)
}
\arguments{
  \item{text}{The text to use for a new status }
  \item{status}{An object of class \code{\link{status}}}
  \item{lat}{If not \code{NULL}, the latitude the status refers to.  Ignored if no \code{long} 
  	parameter is provided}
  \item{long}{If not \code{NULL}, the longitude the status refers to.  Ignored if no \code{lat} 
  	parameter is provided}
  \item{placeID}{If not \code{NULL}, provides a place in the world.  See Twitter documentation for details}
  \item{displayCoords}{Whether or not to put a pin on the exact coordinates a tweet has been sent from, 
        \code{true} or \code{false} if not \code{NULL}}
  \item{inReplyTo}{If not \code{NULL}, denotes the status this is in reply to.  Either an object of
        class \code{\link{status}} or an ID value}
  \item{mediaPath}{If not \code{NULL}, file path to a supported media format (PNG, JPG and GIF) to be included in the status update}
  \item{bypassCharLimit}{If \code{TRUE} will not enforce the incoming tweet is less than 140 characters. This can be useful when dealing with autoshortened links}
  \item{...}{Optional arguments to be passed to \code{\link{GET}}}
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
\examples{
   \dontrun{
      ns <- updateStatus('this is my new status message')
      ## ooops, we want to remove it!
      deleteStatus(ns)
   }
}
\keyword{ interface }
