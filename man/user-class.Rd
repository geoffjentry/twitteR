\name{user-class}
\Rdversion{1.1}
\docType{class}
\alias{userFactory}
\alias{user-class}
\alias{user}
\alias{buildUser}
\alias{screenName}
\alias{screenName,user-method}
\alias{show,user-method}
\alias{as.data.frame,user-method}
\alias{description,user-method}
\alias{statusesCount,user-method}
\alias{statusesCount}
\alias{created}
\alias{description}
\alias{favoritesCount}
\alias{followersCount}
\alias{friendsCount}
\alias{followersCount,user-method}
\alias{favoritesCount,user-method}
\alias{friendsCount,user-method}
\alias{userURL,user-method}
\alias{userURL}
\alias{name,user-method}
\alias{name}
\alias{created,user-method}
\alias{tweetCount,user-method}
\alias{tweetCount}
\alias{protected,user-method}
\alias{protected}
\alias{verified,user-method}
\alias{verified}
\alias{location,user-method}
\alias{location}
\alias{id,user-method}
\alias{lastStatus,user-method}
\alias{lastStatus}
\alias{listedCount,user-method}
\alias{listedCount}
\alias{followRequestSent,user-method}
\alias{followRequestSent}
\alias{profileImageUrl}
\alias{profileImageUrl,user-method}
\title{A container object to model Twitter users }
\description{This class is designed to represent a user on Twitter,
  modeling information available}
\section{Fields}{
  \describe{
    \item{\code{name}:}{Name of the user}
    \item{\code{screenName}:}{Screen name of the user}
    \item{\code{id}:}{ID value for this user}
    \item{\code{lastStatus}:}{Last status update for the user}
    \item{\code{description}:}{User's description}
    \item{\code{statusesCount}:}{Number of status updates this user has
      had}
    \item{\code{followersCount}:}{Number of followers for this user}
    \item{\code{favoritesCount}:}{Number of favorites for this user}
    \item{\code{friendsCount}:}{Number of followees for this user}
    \item{\code{url}:}{A URL associated with this user}
    \item{\code{created}:}{When this user was created}
    \item{\code{protected}:}{Whether or not this user is protected}
    \item{\code{verified}:}{Whether or not this user is verified}
    \item{\code{location}:}{Location of the user}
    \item{\code{listedCount}:}{The number of times this user appears in public lists}
    \item{\code{followRequestSent}:}{If authenticated via OAuth, will be TRUE if you've sent a friend request to this user}
    \item{\code{profileImageUrl}:}{URL of the user's profile image, if one exists}
  }
}
\section{Methods}{
  \describe{
    \item{\code{getFollowerIDs(n=NULL, ...)}:}{Will return a vector of
      twitter user IDs representing followers of this user, up to a
      maximum of \code{n} values.  If \code{n} is NULL, all followers
      will be returned}
    \item{\code{getFollowers(n=NULL, ...)}:}{Will return a list of
     \code{user} objects  representing followers of this user, up to a
      maximum of \code{n} values.  If \code{n} is NULL, all followers
      will be returned}
    \item{\code{getFriendIDs(n=NULL, ...)}:}{Will return a vector of
      twitter user IDs representing users this user follows, up to a
      maximum of \code{n} values.  If \code{n} is NULL, all friends
      will be returned}
    \item{\code{getFriends(n=NULL, ...)}:}{Will return a list of
      \code{user} objects representing users this user follows, up to a
      maximum of \code{n} values.  If \code{n} is NULL, all friendss
      will be returned}
    \item{\code{toDataFrame(row.names=NULL, optional=FALSE)}:}{Converts
      this into a one row
       \code{\link{data.frame}}, with each field except for
       \code{lastStatus} representing a column.
       This can also be accomplished by the S4 style
       \code{as.data.frame(objectName)}.}

  }
}
\details{
  The \code{user} class is implemented as a reference class.  This class
  was previously implemented as an S4 class, and for backward
  compatibility purposes the old S4 accessor methods have been left in,
  although new code should not be written with these.  An instance of a
  generator for this class is provided as a convenience to the user as
  it is configured to handle most standard cases.  To access this
  generator, use the object \code{userFactory}.  Accessor set & get
  methods are provided for every field using reference class
  \code{$accessors()} methodology (see \code{\link{setRefClass}} for
  more details).  As an example, the \code{screenName} field could be
  accessed using \code{object$getScreenName} and
  \code{object$setScreenName}.

  The constructor of this object assumes that the user is passing in a
  JSON encoded Twitter user.  It is also possible to directly pass in
  the arguments.
}
\author{Jeff Gentry}
\seealso{\code{\link{status}}, \code{\link{setRefClass}}}
\examples{
   ## This example is run, but likely not how you want to do things
   us <- userFactory$new(screenName="test", name="Joe Smith")
   us$getScreenName()
   us$getName()

   \dontrun{
     ## Assume 'json' is the return from a Twitter call
     us <- userFactory$new(json)
     us$getScreenName()
   }
}
\keyword{classes}
