\name{getTrends}
\alias{getTrends}
\alias{availableTrendLocations}
\alias{closestTrendLocations}
\title{
Functions to view Twitter trends
}
\description{
  These functions will allow you to interact with the trend portion of
  the Twitter API
}
\usage{
  availableTrendLocations(...)
  closestTrendLocations(lat, long, ...)
  getTrends(woeid, exclude=NULL, ...)
}
\arguments{
  \item{woeid}{A numerical identification code describing a location, a  Yahoo! Where On Earth ID}
  \item{lat}{A numerical latitude value, between -180 and 180 inclusive. West is negative, East is positive}
  \item{long}{A numerical longitude value, between -180 and 180 inclusive. South is negative, North is positive}
  \item{exclude}{If set to \code{hashtags}, will exclude hashtags}
  \item{...}{Additional arguments to be passed to RCurl}
}
\details{
  The \code{availableTrendLocations} and \code{closestTrendLocations} functions will return a 
  data.frame with three columns - \code{name}, \code{country} and \code{woeid}. The \code{closestTrendLocations}
  function will return the locations closest to the specified latitude and longitude.
  
  The \code{getTrends} function takes a specified woeid and returns the trending topics associated
  with that woeid. It returns a data.frame with the columns being \code{name}, \code{url},
  \code{promoted_content}, \code{query} and \code{woeid} - one row per trend.
}
\value{
A data.frame with the columns specified in \code{Details} above
}
\author{
Jeff Gentry
}
\examples{
  \dontrun{
    woeid = availableTrendLocations[1, "woeid"]
    t1 <- getTrends(woeid)
  }
}
\keyword{interface}
