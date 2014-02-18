\name{strip_retweets}
\alias{strip_retweets}
\title{
A function to remove retweets
}
\description{
Given a list of status objects, will remove retweets from the list
to provide a "pure" set of tweets.
}
\usage{
strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE)
}
\arguments{
  \item{tweets}{A list of \code{\link{status}} objects}
  \item{strip_manual}{If \code{TRUE} will remove old style manual retweets}
  \item{strip_mt}{If \code{TRUE} will remove modified tweets (MT)}
}
\details{
Newer style retweets are summarily removed regardless of options.

Older style retweets (aka manual retweets) are tweets of the form 
\code{RT @user blah blah}. If \code{strip_manual} is \code{TRUE}, tweets
containing the \code{RT} string will have everything including and to the
right of the \code{RT} will be removed. Everything to the left of the
\code{RT} will remain, as this should be original content.

If \code{strip_mt} is \code{TRUE}, tweets will be stripped in the same
manner as \code{strip_manual} but using the string {\code{MT}}
}
\value{
A list of \code{status} objects with retweeted content removed
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{status}}
}
\examples{
  \dontrun{
    tweets = searchTwitter("stuff")
    no_retweets = strip_retweets(tweets)
  }
}
\keyword{ utilities }
