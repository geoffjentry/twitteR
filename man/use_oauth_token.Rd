\name{use_oauth_token}
\alias{use_oauth_token}
\title{
Sets up the OAuth credentials for a twitteR session from an existing Token object
}
\description{
This function uses an existing httr OAuth Token in the Twitter session
}
\usage{
use_oauth_token(twitter_token)
}
\arguments{
  \item{twitter_token}{An httr Token object}
}
\details{
  This function is an escape hatch for nonstandard OAuth scenarios. Use setup_twitter_token
  unless it doesn't work for your use case.
}
\value{
  This is called for its side effect
}
\author{
Anand Patil
}
\seealso{
\code{\link{use_twitter_token}}, \code{\link{Token}}
}
\examples{
 \dontrun{
    library(httr)
    library(twitteR)
    token <- Token2.0$new(
      params = list(as_header=TRUE),
      app = oauth_app("fun.with.twitter", "no.key", "no.secret"),
      endpoint = oauth_endpoints("twitter"),
      credentials = list(access_token = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\%2FAAAAAAAAAAAAAAAAAAAA\%3DAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"),
      cache = FALSE
    )

    use_oauth_token(token)
  }
}
\keyword{interface}