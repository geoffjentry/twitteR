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
  This function will store the \code{oauth} argument in an environment
  which is then accessed throughout the package.  When API calls are
  made, instead of going through \code{RCurl} they will go through the
  \code{ROAuth} package.
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
}
\keyword{ interface }
