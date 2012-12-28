\name{twListToDF}
\alias{twListToDF}
\title{
A function to convert twitteR lists to data.frames
}
\description{
This function will take a list of objects from a single twitteR class and return a data.frame
version of the members
}
\usage{
twListToDF(twList)
}
\arguments{
  \item{twList}{A list of objects of a single twitteR class, restrictions are listed in \code{details}}
}
\details{
  The classes supported by this function are \code{\link{status}}, \code{\link{user}},
  and \code{\link{directMessage}}.
}
\value{
A \code{\link{data.frame}} with rows corresponding to the objects in the list and columns being the
fields of the class
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{status}}, \code{\link{user}}, \code{\link{directMessage}}
}
\examples{
  \dontrun{
    zz <- searchTwitter("#rstats")
    twListToDF(zz)
  }
}
\keyword{ interface }
