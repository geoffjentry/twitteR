\name{registerTwitterOAuth}
\alias{registerTwitterOAuth}
\alias{getTwitterOAuth}
\title{ Register OAuth credentials to twitter R session }
\description{
These functions are deprecated
}
\usage{
getTwitterOAuth(consumer_key, consumer_secret)
registerTwitterOAuth(oauth)
}
\arguments{
  \item{consumer_key}{The consumer key supplied by Twitter}
  \item{consumer_secret}{The consumer secret supplied by Twitter}
  \item{oauth}{An object of class \code{OAuth}}
}
\details{
These functions are deprecated, see \code{\link{setup_twitter_oauth}}
}
\value{
  \code{TRUE} on success, otherwise an error will be thrown
}
\author{Jeff Gentry}
\seealso{ \code{setup_twitter_oauth} }
\examples{
  \dontrun{
     fakeExample = 5
  }
}
\keyword{ interface }
