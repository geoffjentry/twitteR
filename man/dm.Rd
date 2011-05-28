\name{dmGet}
\alias{dmGet}
\alias{dmSent}
\alias{dmDestroy}
\alias{dmSend}
\title{ Functions to manipulate Twitter direct messages }
\description{
  These functions allow you to interact with, send, and delete direct
  messages (DMs) in Twitter.
}
\usage{
dmGet(n=25, sinceID=NULL, maxID=NULL, ...)
dmSent(n=25, sinceID=NULL, maxID=NULL, ...)
dmDestroy(dm, ...)
dmSend(text, user, ...)
}
\arguments{
  \item{text}{The text of a message to send}
  \item{user}{The user to send a message to, either \code{character} or
    an \code{\link{user}} object.}
  \item{dm}{The message to delete, an object of class \code{\link{directMessage}}}
  \item{n}{ The maximum number of direct messages to return }
  \item{sinceID}{If not \code{NULL}, an ID representing the earliest
    boundary}
  \item{maxID}{If not \code{NULL}, an ID representing the newest ID you
    wish to retrieve}
  \item{...}{Further arguments to pass along the communication chain}
}
\value{
  These functions will not work without \code{OAuth} authentication

  The \code{dmGet} and \code{dmSent} functions will return a list of
  \code{\link{directMessage}} objects.  The former will retrieve DMs
  sent to the user while the latter retrieves messages sent from the user.

  The \code{dmDestroy} function takes a \code{\link{directMessage}}
  object (perhaps from either \code{dmGet} or \code{dmSent}) and will
  delete it from the Twitter server.

  The \code{dmSend} function will send a message to another Twitter user.
}
\author{Jeff Gentry}
\seealso{\code{\link{directMessage}}, \code{\link{registerTwitterOAuth}}}
\examples{
  \dontrun{
           dms <- dmGet()
           dms
           ## delete the first one
           dms[[1]]$destroy()
           dmDestroy(dms[[2]])
           ## send a DM
           dmSend('Testing out twitteR!', 'twitter')
   }
}
\keyword{ interface }

