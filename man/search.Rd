\name{searchTwitter}
\alias{searchTwitter}
\alias{Rtweets}
\title{ Search twitter }
\description{
  This function will issue a search of Twitter based on a supplied
  search string.
}
\usage{
searchTwitter(searchString, n=25, lang=NULL, since=NULL, ...)
Rtweets(n=25, lang=NULL, since=NULL, ...)
}
\arguments{
  \item{searchString}{Search query to issue to twitter}
  \item{n}{The number of tweets to return}
  \item{lang}{If not \code{NULL}, restricts tweets to the given language, given by an ISO 639-1 code}
  \item{since}{If not \code{NULL}, restricts tweets to those since the given date.  Date is to be formatted as YYYY-MM-DD}
  \item{...}{Optional arguments to be passed to \code{\link{getURL}}}
}
\details{
  These commands will return any authorized tweets which match the
  search criteria.  Note that there are pagination restrictions as well
  as other limits on what can be searched, so it is always possible to
  not retrieve as many tweets as was requested with the \code{n}
  argument.  Authorized tweets are public tweets as well as those
  protected tweets that are available to the user after authenticating
  via \code{\link{registerOAuth}}.

  The \code{Rtweets} function is a wrapper around \code{searchTwitter}
  which hardcodes in a search for \code{#rstats}.
  
  A premade session will almost never be necessary.
  
}
\value{
  A list of \code{\link{status}} objects
}
\author{Jeff Gentry}
\seealso{ \code{\link{status}} }
\examples{
	searchTwitter("#beer", n=100)
        Rtweets(n=37)
}
\keyword{ interface }
