\name{taskStatus}
\alias{taskStatus}
\title{
  A function to send a Twitter DM after completion of a task
}
\description{
  This function will run an R expression and send a direct message to a
  specified user on success or failure.
}
\usage{
    taskStatus(expr, to, msg="")
}
\arguments{
  \item{expr}{An R expression that will be run}
  \item{to}{The user to send a message to, either \code{character} or
    an \code{\link{user}} object.}
  \item{msg}{An extra message to append to the standard DM}
}
\details{
  This function will run \code{expr}, and send a Direct Message (DM)
  upon completion which will report the expression's success or failure.
}
\value{
  Either the value of the expression or an object of class
  \code{try-error}.
}
\author{Jeff Gentry}
\seealso{\code{\link{dmSend}}}
\examples{
   \dontrun{
       taskStatus(z<-5, "username", session=sess)
   }
}
\keyword{interface}
