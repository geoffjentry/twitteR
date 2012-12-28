\name{friendships}
\alias{friendships}
\title{
A function to detail relations between yourself & other users
}
\description{
This function will accept a list of other Twitter users and will detail if
they follow you and/or you follow them.
}
\usage{
friendships(screen_names = character(), user_ids = character(), ...)
}
\arguments{
  \item{screen_names}{
    A vector of one or more Twitter screen names
  }
  \item{user_ids}{
    A vector of one or more Twitter user id values
  }
  \item{\dots}{
    Any other arguments to pass to RCurl
  }
}
\details{
  The combined number of screen names and user ids may not exceed 100. Any
  non-existent users will be dropped from the output
}
\value{
A data.frame, one row for each user requested with columns \code{name},
\code{screen_name}, \code{id}, \code{following} and \code{followed_by}. The
latter two columns will be \code{TRUE} or \code{FALSE} depending on that user's
relations with your account.
}
\references{
https://dev.twitter.com/docs/api/1.1/get/friendships/lookup
}
\author{
Jeff Gentry
}
\seealso{
\code{\link{registerTwitterOAuth}}
}
\examples{
  \dontrun{
    friendships()
  }
}
\keyword{ interface }
