\name{twitteR-defunct}
\alias{initSession}
\alias{friendsTimeline}
\alias{mentions}
\alias{updateStatus}
\alias{tweet}
\alias{deleteStatus}
\alias{dmGet}
\alias{dmSent}
\alias{dmDestroy}
\alias{dmSend}
\alias{taskStatus}
\alias{twFromJSON}
\title{ Defunct functions }
\description{
  As of July 2010, Twitter has changed their authentication structure to
  require OAuth.  Until such a time that OAuth authentication is enabled
  in R and/or this requirement changes, these functions will not work properly.
}
\usage{
initSession(username, password)
dmGet(session = getCurlHandle(), num=20, ...)
dmSent(session = getCurlHandle(), ...)
dmDestroy(dm, session = getCurlHandle(), ...)
dmSend(text, user, session = getCurlHandle(), ...)
tweet(text, session = getCurlHandle(), ...)
updateStatus(text, session = getCurlHandle(), ...)
deleteStatus(status, session = getCurlHandle(), ...)
taskStatus(expr, to, msg="", session=getCurlHandle())
friendsTimeline(session=getCurlHandle(), ...)
mentions(session=getCurlHandle(), ...)
}
\arguments{
  \item{username}{ Your Twitter screenname }
  \item{password}{ Your Twitter password }
  \item{text}{The text of a message to send}
  \item{user}{The user to send a message to, either \code{character} or
    an \code{\link{user}} object.}
  \item{dm}{The message to delete, an object of class \code{directMessage}}
  \item{session}{ A session object from \code{\link{initSession}}}
  \item{num}{ The number of direct messages to return }
  \item{status}{An object of class \code{\link{status}}}
  \item{expr}{An R expression that will be run}
  \item{to}{The user to send a message to, either \code{character} or
    an \code{\link{user}} object.}
  \item{msg}{An extra message to append to the standard DM}
  \item{...}{Optional arguments to be passed to \code{\link{getURL}}}

}
\author{Jeff Gentry}
\keyword{ interface }
