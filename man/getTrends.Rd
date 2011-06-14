\name{getTrends}
\alias{getTrends}
\title{
Functions to view Twitter trends
}
\description{
  These functions will allow you to interact with the trend portion of
  the Twitter API
}
\usage{
getTrends(period = c("current", "daily", "weekly"), exclude = NULL, date = NULL)
}
\arguments{
  \item{period}{One of \code{current}, \code{daily}, or \code{weekly},
    to describe the time period to acquire data for}
  \item{exclude}{If set to \code{hashtags}, will exclude hashtags}
  \item{date}{For periods \code{daily} and \code{weekly}, the date to
    use as a starting point, in the format \code{YYYY-MM-DD}.  Note that
    going back too far (roughly 10-14 days) will result in an empty result}
}
\details{
  The \code{current} period will return the 10 current trending topics
  on Twitter.

  The \code{daily} period will return the top 20 trending topics per
  hour for the given date

  The \code{weekly} period will return the top 30 trending topics for
  each day of the week starting with \code{date}
}
\value{
  A list of \code{\link{trend}} objects
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{trend}}
}
\examples{
   t1 <- getTrends()
   t2 <- getTrends('weekly', as.character(Sys.Date()-1))
}
\keyword{interface}
