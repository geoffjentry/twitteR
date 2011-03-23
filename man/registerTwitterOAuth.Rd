\name{registerTwitterOAuth}
\alias{registerTwitterOAuth}
\alias{exampleOauth}
\title{ Register OAuth credentials to twitter R session }
\description{
  This function is used to provide your OAuth access tokens to
  your twitter session.  This will enable many bits of functionality as
  well as allow other commands to provide more options
}
\usage{
registerTwitterOAuth(oauth)
}
\arguments{
  \item{oauth}{An object of class \code{\link{OAuth}}}
}
\details{
  This function will store the \code{OAuth} argument in an environment
  which is then accessed throughout the package.  When API calls are
  made, instead of going through \code{RCurl} they will go through the
  \code{ROAuth} package.

  Three URLs will need to be used for the initial \code{OAuth}
  handshake, see the examples below.
}
\value{
  \code{TRUE} on success, otherwise an error will be thrown
}
\author{Jeff Gentry}
\seealso{ \code{\link{OAuth}} }
\examples{
    ## Note that exampleOauth isn't a valid Twitter credential, but
    ## is a valid OAuth credential
    data(exampleOauth)
    registerTwitterOAuth(exampleOauth)

    \dontrun{
       ## A real example, using a fictitious consumerkey and consumer
       ## secret
       requestURL <- "https://api.twitter.com/oauth/request_token"
       accessURL = "http://api.twitter.com/oauth/access_token"
       authURL = "http://api.twitter.com/oauth/authorize"
       twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                                    consumerSecret=consumerSecret,
                                    requestURL=reqURL,
                                    accessURL=accessURL,
                                    authURL=authURL)
       twitCred$handshake()
       registerTWitterOAuth(twitCred)
    }
}
\keyword{ interface }
