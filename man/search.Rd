\name{searchTwitter}
\alias{searchTwitter}
\alias{Rtweets}
\alias{searchTwitteR}
\title{ Search twitter }
\description{
  This function will issue a search of Twitter based on a supplied
  search string.
}
\usage{
searchTwitter(searchString, n=25, lang=NULL, since=NULL, until=NULL,
              locale=NULL, geocode=NULL, sinceID=NULL, 
	            retryOnRateLimit=120, ...)
Rtweets(n=25, lang=NULL, since=NULL, ...)
}
\arguments{
  \item{searchString}{Search query to issue to twitter}
  \item{n}{The maximum number of tweets to return}
  \item{lang}{If not \code{NULL}, restricts tweets to the given
    language, given by an ISO 639-1 code} 
  \item{since}{If not \code{NULL}, restricts tweets to those since the
    given date.  Date is to be formatted as YYYY-MM-DD} 
  \item{until}{If not \code{NULL}, restricts tweets to those up until the
    given date.  Date is to be formatted as YYYY-MM-DD} 
  \item{locale}{If not \code{NULL}, will set the locale for the search.
    As of 03/06/11 only \code{ja} is effective, as per the Twitter API}
  \item{geocode}{If not \code{NULL}, returns tweets by users located
    within a given radius of the given latitude/longitude. See
    \code{Details} below for more information}
  \item{sinceID}{If not \code{NULL}, returns tweets with IDs greater
    (ie newer) than the specified ID}
  \item{retryOnRateLimit}{If non-zero the search command will block retry up to
    X times if the rate limit is experienced. This might lead to a much longer run
    time but the task will eventually complete if the retry count is high enough}
  \item{...}{Optional arguments to be passed to \code{\link{getURL}}}
}
\details{
  These commands will return any authorized tweets which match the
  search criteria.  Note that there are pagination restrictions as well
  as other limits on what can be searched, so it is always possible to
  not retrieve as many tweets as was requested with the \code{n}
  argument.  Authorized tweets are public tweets as well as those
  protected tweets that are available to the user after authenticating
  via \code{\link{registerTwitterOAuth}}.

  For the \code{geocode} argument, the values are given in the format
  \code{latitude,longitude,radius}, where the radius can have either
  \code{mi} (miles) or \code{km} (kilometers) as a unit.  For example
  \code{geocode='37.781157,-122.39720,1mi'}. 

  For the \code{sinceID} argument, if the requested ID value is older
  than the oldest available tweets, the API will return tweets starting
  from the oldest ID available.
  
  The \code{Rtweets} function is a wrapper around \code{searchTwitter}
  which hardcodes in a search for \code{#rstats}.
}
\value{
  A list of \code{\link{status}} objects
}
\author{Jeff Gentry}
\seealso{ \code{\link{status}}, \code{\link{registerTwitterOAuth}} }
\examples{
  \dontrun{
  	searchTwitter("#beer", n=100)
          Rtweets(n=37)
  
  	## Search between two dates
          searchTwitter('charlie sheen', since='2011-03-01', until='2011-03-02')
  
  	## geocoded results
  	searchTwitter('patriots', geocode='42.375,-71.1061111,10mi')
  }
}
\keyword{ interface }
