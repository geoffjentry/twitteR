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
dmGet(session = getCurlHandle(), num=20, ...)
dmSent(session = getCurlHandle(), ...)
dmDestroy(dm, session = getCurlHandle(), ...)
dmSend(text, user, session = getCurlHandle(), ...)
}
\arguments{
  \item{text}{The text of a message to send}
  \item{user}{The user to send a message to, either \code{character} or
    an \code{\link{user}} object.}
  \item{dm}{The message to delete, an object of class \code{\link{directMessage}}}
  \item{session}{ A session object from \code{\link{initSession}}}
  \item{num}{ The number of direct messages to return }
  \item{...}{Optional arguments to be passed to \code{\link{getURL}}}
}
\value{
  The \code{dmGet} and \code{dmSent} functions will return a list of
  \code{\link{directMessage}} objects.  The former will retrieve DMs
  sent to the user while the latter retrieves messages sent from the user.

  The \code{dmDestroy} function takes a \code{\link{directMessage}}
  object (perhaps from either \code{dmGet} or \code{dmSent}) and will
  delete it from the Twitter server.

  The \code{dmSend} function will send a message to another Twitter user.
}
\author{Jeff Gentry}
\seealso{\code{\link{directMessage}}}
\examples{
  \dontrun{
           sess <- initSession('myuser', 'mypass')
           dms <- dmGet(sess)
           dms
           ## delete the first one
           dmDestroy(dms[[1]])
           ## send a DM
           dmSend('Testing out twitteR!', 'twitter', sess)
   }
}
\keyword{ interface }

