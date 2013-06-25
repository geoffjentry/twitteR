\name{decode_short_url}
\alias{decode_short_url}
\title{
A function to decode shortened URLs
}
\description{
Will expand a URL that has been processed by a link shortener (e.g. bit.ly). Provided as a convenience
function to users who may which to perform this operation.
}
\usage{
decode_short_url(url, ...)
}
\arguments{
  \item{url}{A character string, the URL to decode}
  \item{\dots}{Optional arguments to pass along to RCurl}
}
\details{
Uses the \url{longapi.org} API
}
\value{
A character string containing either the original URL (if not shortened) or the full URL (if shortened)
}
\references{
\url{longapi.org}
}
\author{
Neil Jang
}
\examples{
  \dontrun{
    decode_short_url("http://bit.ly/23226se656")
  }
}
\keyword{utilities}
