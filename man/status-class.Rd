\name{status-class}
\Rdversion{1.1}
\docType{class}
\alias{status-class}
\alias{statusFactory}
\alias{status}
\alias{buildStatus}
\alias{show,status-method}
\alias{as.data.frame,status-method}
\alias{text,status-method}
\alias{favorited,status-method}
\alias{favorited}
\alias{replyToSN,status-method}
\alias{replyToSN}
\alias{created,status-method}
\alias{truncated,status-method}
\alias{truncated}
\alias{replyToSID,status-method}
\alias{replyToSID}
\alias{id,status-method}
\alias{id}
\alias{replyToUID,status-method}
\alias{replyToUID}
\alias{statusSource,status-method}
\alias{statusSource}
\alias{screenName,status-method}
\alias{statusText}
\alias{statusText,status-method}
\alias{retweetCount,status-method}
\alias{retweetCount}
\alias{retweeted,status-method}
\alias{retweeted}
\alias{[[,twitterObjList-method}
\alias{as.data.frame,twitterObj-method}
\alias{show,twitterObjList-method}
\title{Class to contain a Twitter status}
\description{Container for Twitter status messages, including the text
  as well as basic information}
\section{Fields}{
  \describe{
    \item{\code{text}:}{The text of the status}
    \item{\code{screenName}:}{Screen name of the user who posted this status}
    \item{\code{id}:}{ID of this status}
    \item{\code{replyToSN}:}{Screen name of the user this is in reply
      to} 
    \item{\code{replyToUID}:}{ID of the user this was in reply to}
    \item{\code{statusSource}:}{Source user agent for this tweet}
    \item{\code{created}:}{When this status was created}
    \item{\code{truncated}:}{Whether this status was truncated}
    \item{\code{favorited}:}{Whether this status has been favorited}
    \item{\code{retweeted}:}{TRUE if this status has been retweeted}
    \item{\code{retweetCount}:}{The number of times this status has been retweeted}
  }
}
\section{Methods}{
  \describe{
     \item{\code{toDataFrame}:}{Converts this into a one row
       \code{\link{data.frame}}, with each field representing a column.
       This can also be accomplished by the S4 style
       \code{as.data.frame(objectName)}.}
  }
}
\details{
  The \code{status} class is implemented as a reference class.  This class
  was previously implemented as an S4 class, and for backward
  compatibility purposes the old S4 accessor methods have been left in,
  although new code should not be written with these.  An instance of a
  generator for this class is provided as a convenience to the user as
  it is configured to handle most standard cases.  To access this
  generator, use the object \code{statusFactory}.  Accessor set & get
  methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}} for
  more details).  As an example, the \code{screenName} field could be
  accessed using \code{object$getScreenName} and
  \code{object$setScreenName}.

  The constructor of this object assumes that the user is passing in a
  JSON encoded Twitter status.  It is also possible to directly pass in
  the arguments.

}
\author{Jeff Gentry}
\seealso{
  \code{\link{userTimeline}}, \code{\link{setRefClass}}
}
\examples{
   \dontrun{
     st <- statusFactory$new(screenName="test", text="test message")
     st$getScreenName()
     st$getText()

     ## Assume 'json' is the return from a Twitter call
     st <- statusFactory$new(json)
     st$getScreenName()
   }
   
}
\keyword{classes}
