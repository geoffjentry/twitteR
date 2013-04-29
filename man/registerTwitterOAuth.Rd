\name{registerTwitterOAuth}
\alias{registerTwitterOAuth}
\alias{getTwitterOAuth}
\title{ Register OAuth credentials to twitter R session }
\description{
  This function is used to provide your OAuth access tokens to
  your twitter session.  This will enable many bits of functionality as
  well as allow other commands to provide more options
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
  The \code{getTwitterOAuth} function is a wrapper around the call to \code{OAuthFactory}
  and \code{registerTwitterOAuth}, which will return the registered credentials. If 
  your workflow is such that you save the credentials and register them in later R
  sessions, feel free to do this using \code{registerTwitterOAuth}

  \code{registerTwitterOAuth} will store the \code{OAuth} argument in an environment
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
\seealso{ \code{OAuth} }
\examples{
    \dontrun{
       ## A real example, but using a fictitious consumerkey and consumer
       ## secret - you'll need to supply your own
       reqURL <- "https://api.twitter.com/oauth/request_token"
       accessURL <- "http://api.twitter.com/oauth/access_token"
       authURL <- "http://api.twitter.com/oauth/authorize"
       consumerKey <- "12345pqrst6789ABCD"
       consumerSecret <- "abcd1234EFGH5678ijkl0987MNOP6543qrst21"
       twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                                    consumerSecret=consumerSecret,
                                    requestURL=reqURL,
                                    accessURL=accessURL,
                                    authURL=authURL)
       twitCred$handshake()
       registerTwitterOAuth(twitCred)
    }
}
\keyword{ interface }
