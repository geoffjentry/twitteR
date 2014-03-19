\name{setup_twitter_oauth}
\alias{setup_twitter_oauth}
\title{
Sets up the OAuth credentials for a twitteR session
}
\description{
This function wraps the OAuth authentication handshake functions from the
httr package for a twitteR session
}
\usage{
setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)
}
\arguments{
  \item{consumer_key}{The consumer key supplied by Twitter}
  \item{consumer_secret}{The consumer secret supplied by Twitter}
  \item{access_token}{The access token supplied by Twitter}
  \item{access_secret}{The access secret supplied by Twitter}
}
\details{
  The \code{httr} package can cache authentication. See \code{\link{Token}} for
  details

  If both \code{access_token} and \code{access_secret} are set (i.e. not \code{NULL}),
  these will be supplied directly to the OAuth authentication instead of the browser
  based authentication dance one would normally experience. This requires you to already
  know the access tokens for your Twitter app. The usefuleness of this feature is primarily
  in a headless environment where a web browser is not available.
}
\value{
   This is called for its side effect
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{Token}}, \code{\link{GET}}, \code{\link{POST}}
}
\examples{
 \dontrun{
    setup_twitter_oauth("CONSUMER_KEY", "CONSUMER_SECRET")
 }
}
\keyword{interface}