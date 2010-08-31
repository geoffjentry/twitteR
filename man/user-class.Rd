\name{user-class}
\Rdversion{1.1}
\docType{class}
\alias{user-class}
\alias{user}
\alias{buildUser}
\alias{screenName}
\alias{screenName,user-method}
\alias{show,user-method}
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
\title{A container object to model Twitter users }
\description{This class is designed to represent a user on Twitter,
  modeling information available}
\section{Objects from the Class}{
  \code{new('user',
    description = [character],
    statusesCount = [numeric],
    followersCount = [numeric],
    favoritesCount = [numeric],
    favoritesCount = [numeric],
    friendsCount = [numeric],
    url = [character],
    name = [character],
    created = [POSIXct],
    protected = [logical],
    verified = [logical],
    screenName = [character],
    location = [character],
    id = [numeric],
    lastStatus = [status], ...)}
}
\section{Slots}{
	 \describe{
    \item{\code{description}:}{Description of the user }
    \item{\code{statusesCount}:}{Number of status messages the user has }
    \item{\code{followersCount}:}{Number of followers the user has }
    \item{\code{favoritesCount}:}{Number of favorites the user has }
    \item{\code{friendsCount}:}{The number of friends the user has }
    \item{\code{url}:}{URL associated with the user }
    \item{\code{name}:}{The user's name }
    \item{\code{created}:}{When the user was created }
    \item{\code{protected}:}{Is the user protected? }
    \item{\code{verified}:}{Is the user verified? }
    \item{\code{screenName}:}{Screen name of the user }
    \item{\code{location}:}{Location of the user }
    \item{\code{id}:}{ID of the user }
    \item{\code{lastStatus}:}{A \code{"status"} object displaying the
      last status the user has posted }
  }
}
\section{Methods}{
  \describe{
    \item{screenName}{Returns the screen name of the user }
    \item{show}{Displays basic information about the user }
    \item{description}{Returns the user's description}
    \item{statusesCount}{Returns the number of tweets for the user}
    \item{followersCount}{Returns the number of followers the user has }
    \item{friendsCount}{Returns the number of friends for the user}
    \item{userURL}{Retrieves the URL associated with the user}
    \item{name}{Retrieves the user's name}
    \item{created}{Returns the creation time of the user}
    \item{protected}{Returns the protection status of the user's
      timeline}
    \item{verified}{Returns the verified status of the user}
    \item{location}{Returns the location of the user}
    \item{id}{Returns the user's ID}
    \item{lastStatus}{Returns the last tweet the user posted}
  }
}
\author{Jeff Gentry}
\keyword{classes}
