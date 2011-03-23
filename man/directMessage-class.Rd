\name{directMessage-class}
\Rdversion{1.1}
\docType{class}
\alias{directMessage-class}
\alias{directMessage}
\alias{dmFactory}
\alias{show,directMessage-method}
\title{Class "directMessage":  A class to represent Twitter Direct Messages }
\description{ Provides a model representing direct messages (DMs) from Twitter}
\section{Fields}{
  \describe{
     \item{\code{text}:}{Text of the DM}
     \item{\code{recipient}:}{A \code{user} object representing the
       recipient of the message} 
     \item{\code{recipientSN}:}{Screen name of the recipient}
     \item{\code{recipientID}:}{ID number of the recipient}
     \item{\code{sender}:}{A \code{user} object representing the sender
       of the message} 
     \item{\code{senderSN}:}{Screen name of the sender}
     \item{\code{senderID}:}{ID number of the sender}
     \item{\code{created}:}{When the messages was created}
  }
}
\section{Methods}{
  \describe{
     \item{\code{destroy}:}{Deletes this DM from Twitter.  A wrapper
       around \code{\link{dmDestroy}}} 
  }
}
\details{
  The \code{directMessage} class is implemented as a reference class.
  As there should be no backwards compatibility issues, there are no S4
  methods provided as with the \code{user} and \code{status} classes.
  An instance of a generator for this class is provided as a convenience
  to the user as it is configured to handle most standard cases.  To
  access this generator, use the object \code{dmFactory}.  Accessor set
  & get methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}} for
  more details).  As an example, the \code{sender} field could be
  accessed using \code{object$getSender()} and \code{object$setSender()}.

  The constructor of this object assumes that the user is passing in a
  JSON encoded Twitter Direct Message.  It is also possible to directly
  pass in the arguments. 
}
\author{ Jeff Gentry }
\seealso{\code{\link{dmGet}}, \code{\link{dmSend}}, \code{\link{dmDestroy}}, \code{\link{setRefClass}}}
\examples{
  ## This example is run, but likely not how you want to do things
  dm <- dmFactory$new(text='foo', recipientSN='blah')
  dm$getText()
  
  \dontrun{
    ## assume 'json' is the return from a Twitter call
    dm <- dmFactory$new(json)
    dm$getSenderID()
  }
}
\keyword{classes}
