\name{setup_twitter_oauth}
\alias{setup_twitter_oauth}
\alias{load_twitter_oauth}
\title{
Functions to register OAuth credentials for an R twitter session
}
\description{
These functions are used to manage your OAuth access tokens for a twitter session.
Credentials can be saved for a limited time and reloaded in a future session
}
\usage{
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret, credentials_file = NULL)
load_twitter_oauth(credentials_file)
}
\arguments{
  \item{consumer_key}{The consumer key supplied by Twitter}
  \item{consumer_secret}{The consumer secret supplied by Twitter}
  \item{access_token}{The access token supplied by Twitter}
  \item{access_secret}{The access secret supplied by Twitter}
  \item{credentials_file}{A file to save/load authenticated credentials to/from}
}
\details{
  If \code{credentials_file} is non-\code{NULL} in \code{setup_twitter_oauth}, the 
  authenticated credentials will be saved to this file name. These can then be reloaded into 
  an \R session via \code{load_twitter_oauth}.
}
\value{
   These functions are called for their side effects
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{GET}}, \code{\link{POST}}
}
\examples{
 \dontrun{
    setup_twitter_oauth("CONSUMER_KEY", "CONSUMER_SECRET", "ACCESS_TOKEN", "ACCESS_SECRET", 
                         credentials_file="creds.rda")
    load_twitter_oauth("creds.rda")
 }
}
\keyword{interface}