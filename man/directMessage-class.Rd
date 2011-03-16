\name{directMessage-class}
\Rdversion{1.1}
\docType{class}
\alias{directMessage-class}
\alias{directMessage}
\alias{show,directMessage-method}
\alias{created,directMessage-method}
\alias{text,directMessage-method}
\alias{recipientSN,directMessage-method}
\alias{recipientSN}
\alias{sender,directMessage-method}
\alias{sender}
\alias{recipient,directMessage-method}
\alias{recipient}
\alias{recipientID,directMessage-method}
\alias{recipientID}
\alias{senderID,directMessage-method}
\alias{senderID}
\alias{id,directMessage-method}
\alias{senderSN,directMessage-method}
\alias{senderSN}
\alias{buildDM}
\title{Class "directMessage" }
\description{ Provides a model representing direct messages (DMs) from Twitter}
\section{Objects from the Class}{
  \code{new('directMessage',
    text = [character],
    recipientSN = [character],
    created = [character],
    recipientID = [numeric],
    sender = [user],
    recipient = [user],
    senderID = [numeric],
    id = [numeric],
    senderSN = [character],
    ...)
  }

  \code{directMessage} instances are usually created through calls to
  the \code{\link{dmGet}} function.
}
\section{Slots}{
	 \describe{
    \item{\code{text}:}{The text of the direct message}
    \item{\code{recipientSN}:}{Screen name of the recipient }
    \item{\code{created}:}{When this message was created }
    \item{\code{recipientID}:}{ID of the recipient }
    \item{\code{sender}:}{An object of class \code{user} representing
      the sender }
    \item{\code{recipient}:}{And object of class \code{user}
      representing the recipient }
    \item{\code{senderID}:}{The ID of the sender }
    \item{\code{id}:}{Internal ID of this message }
    \item{\code{senderSN}:}{Screen name of teh sender }
  }
}
\section{Methods}{
  \describe{
    \item{show}{Displays the DM}
    \item{text}{Retrieves the text of the DM}
    \item{recipientSN}{Retrieves the screen name of the recipient}
    \item{created}{Retrieves the creation date}
    \item{recipientID}{Retrieves the ID of the recipient}
    \item{sender}{Retrieves the sender (of class \code{\link{user}})
      of the DM}
    \item{recipient}{Retrieves the recipient (of class
      \code{\link{user}}) of the DM}
    \item{senderID}{Retrives the ID of the DM sender}
    \item{id}{Retrieves the ID of the DM}
    \item{senderSN}{Retrieves the screen name of the sender}
  }
}
\author{ Jeff Gentry }
\seealso{\code{\link{dmGet}}}
\keyword{classes}
