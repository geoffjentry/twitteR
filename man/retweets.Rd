\name{retweets}
\alias{retweets}
\alias{retweeters}
\title{
Functions to work with retweets
}
\description{
These functions can be used to return retweets or users who retweeted a tweet
}
\usage{
retweets(id, n = 20, ...)
}
\arguments{
  \item{id}{The ID of the tweet to get retweet information on}
  \item{n}{The number of results to return, up to 100}
  \item{\dots}{Further arguments to pass on to httr}
}
\value{
   For \code{retweets} the n most recent retweets of the original tweet.
   
   For \code{retweeters} the n most recent users who have retweeted this tweet.
}
\author{
Jeff Gentry
}
\seealso{
  \code{\link{showStatus}}
}
\examples{
  \dontrun{
     retweets("21947795900469248")
     
     st = showStatus("21947795900469248")
     retweeters(st$getId())
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
