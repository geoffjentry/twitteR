setRefClass('userList',
            contains='twitterObjList'
            )

setValidity('userList', function(object) {
  listClassValidity(object, 'user')
})

#' A container object to model Twitter users
#' 
#' This class is designed to represent a user on Twitter, modeling information
#' available
#' 
#' The \code{user} class is implemented as a reference class.  This class was
#' previously implemented as an S4 class, and for backward compatibility
#' purposes the old S4 accessor methods have been left in, although new code
#' should not be written with these.  An instance of a generator for this class
#' is provided as a convenience to the user as it is configured to handle most
#' standard cases.  To access this generator, user the object
#' \code{userFactory}.  Accessor set & get methods are provided for every field
#' using reference class \code{$accessors()} methodology (see
#' \code{\link{setRefClass}} for more details).  As an example, the
#' \code{screenName} field could be accessed using \code{object$getScreenName}
#' and \code{object$setScreenName}.
#' 
#' The constructor of this object assumes that the user is passing in a JSON
#' encoded Twitter user.  It is also possible to directly pass in the
#' arguments.
#' 
#' @name user-class
#' @aliases userFactory user-class user buildUser screenName
#' screenName,user-method show,user-method as.data.frame,user-method
#' description,user-method statusesCount,user-method statusesCount created
#' description favoritesCount followersCount friendsCount
#' followersCount,user-method favoritesCount,user-method
#' friendsCount,user-method userURL,user-method userURL name,user-method name
#' created,user-method tweetCount,user-method tweetCount protected,user-method
#' protected verified,user-method verified location,user-method location
#' id,user-method lastStatus,user-method lastStatus listedCount,user-method
#' listedCount followRequestSent,user-method followRequestSent profileImageUrl
#' profileImageUrl,user-method
#' @docType class
#' @section Fields: \describe{ \item{list("name")}{Name of the
#' user}\item{:}{Name of the user} \item{list("screenName")}{Screen name of the
#' user}\item{:}{Screen name of the user} \item{list("id")}{ID value for this
#' user}\item{:}{ID value for this user} \item{list("lastStatus")}{Last status
#' update for the user}\item{:}{Last status update for the user}
#' \item{list("description")}{User's description}\item{:}{User's description}
#' \item{list("statusesCount")}{Number of status updates this user has
#' had}\item{:}{Number of status updates this user has had}
#' \item{list("followersCount")}{Number of followers for this
#' user}\item{:}{Number of followers for this user}
#' \item{list("favoritesCount")}{Number of favorites for this
#' user}\item{:}{Number of favorites for this user}
#' \item{list("friendsCount")}{Number of followees for this
#' user}\item{:}{Number of followees for this user} \item{list("url")}{A URL
#' associated with this user}\item{:}{A URL associated with this user}
#' \item{list("created")}{When this user was created}\item{:}{When this user
#' was created} \item{list("protected")}{Whether or not this user is
#' protected}\item{:}{Whether or not this user is protected}
#' \item{list("verified")}{Whether or not this user is
#' verified}\item{:}{Whether or not this user is verified}
#' \item{list("location")}{Location of the user}\item{:}{Location of the user}
#' \item{list("listedCount")}{The number of times this user appears in public
#' lists}\item{:}{The number of times this user appears in public lists}
#' \item{list("followRequestSent")}{If authenticated via OAuth, will be TRUE if
#' you've sent a friend request to this user}\item{:}{If authenticated via
#' OAuth, will be TRUE if you've sent a friend request to this user}
#' \item{list("profileImageUrl")}{URL of the user's profile image, if one
#' exists}\item{:}{URL of the user's profile image, if one exists} }
#' @author Jeff Gentry
#' @seealso \code{\link{status}}, \code{\link{setRefClass}}
#' @keywords classes
#' @examples
#' 
#'    ## This example is run, but likely not how you want to do things
#'    us <- userFactory$new(screenName="test", name="Joe Smith")
#'    us$getScreenName()
#'    us$getName()
#' 
#'    \dontrun{
#'      ## Assume 'json' is the return from a Twitter call
#'      us <- userFactory$new(json)
#'      us$getScreenName()
#'    }
#'
setRefClass("user",
            contains='twitterObj',
            fields = list(
              description="character",
              statusesCount="numeric",
              followersCount="numeric",
              favoritesCount="numeric",
              friendsCount="numeric",
              url="character",
              name="character",
              created="POSIXct",
              protected="logical",
              verified="logical",
              screenName="character",
              location="character",
              lang="character",
              id="character",
              lastStatus="status",
              listedCount="numeric",
              followRequestSent="logical",
              profileImageUrl="character"
              ),
            methods = list(
              initialize = function(json, ...) {
                if (!missing(json)) {
                  if (!is.null(json[['status']]))
                    lastStatus <<- buildStatus(json[['status']])
                  
                  if (is.character(json[['description']]))
                    description <<- json[['description']]
                  
                  if (!is.null(json[['statuses_count']]))
                    statusesCount <<- as.numeric(json[['statuses_count']])
                  
                  if (!is.null(json[['followers_count']]))
                    followersCount <<- as.numeric(json[['followers_count']])
                  
                  if (!is.null(json[['friends_count']]))
                    friendsCount <<- as.numeric(json[['friends_count']])

                  ## NOTE: Twitter uses the british spelling for historical reasons
                  favorites_count = get_json_value(json, c("favourites_count", "favorites_count"))
                  if (!is.null(favorites_count)) {
                    favoritesCount <<- as.numeric(favorites_count)
                  }
                                    
                  if ((!is.null(json[['url']]))&&(!is.na(json[['url']])))
                    url <<- json[['url']]

                  if (is.character(json[['name']]))
                    name <<- json[['name']]
                  
                  created_at = get_json_value(json, c("created_at", "created"))
                  if (is.null(created_at)) {
                    created <<- Sys.time()
                  } else {
                    created <<- twitterDateToPOSIX(created_at)
                  }
                  
                  if ((is.null(json[['protected']])) ||
                      (json[['protected']] == FALSE))
                    protected <<- FALSE
                  else
                    protected <<- TRUE

                  if ((is.null(json[['verified']])) ||
                      (json[['verified']] == FALSE))
                    verified <<- FALSE
                  else
                    verified <<- TRUE

                  if (is.character(json[['screen_name']]))
                    screenName <<- json[['screen_name']]
                  
                  # Note: id_str must be checked first!
                  id_field = get_json_value(json, c("id_str", "id"))
                  if (!is.null(id_field)) {
                    id <<- as.character(id_field)
                  }
                  
                  if (!is.null(json[['location']])) {
                    location <<- json[['location']]
                  }

                  if (!is.null(json[['lang']])) {
                    lang <<- json[['lang']]
                  }
                  
                  if (!is.null(json[["listed_count"]])) {
                    listedCount <<- json[["listed_count"]]
                  }
                  
                  if ((is.null(json[["followRequestSent"]])) ||
                      (json[["followRequestSent"]] == FALSE)) {
                    followRequestSent <<- FALSE
                  } else {
                    followRequestSent <<- TRUE
                  }
                  
                  if (!is.null(json[["profile_image_url"]])) {
                    profileImageUrl <<- json[["profile_image_url"]]
                  }
                }
                
                callSuper(...)
              },
              getFollowerIDs = function(n=NULL, ...) {
                return(unlist(followers(.self$id, n, ...)))
              },
              getFollowers = function(n=NULL, ...) {
                fol <- .self$getFollowerIDs(n, ...)
                lookupUsers(fol, ...)
              },
              getFriendIDs = function(n=NULL, ...) {
                return(unlist(friends(.self$id, n, ...)))
              }, 
              getFriends = function(n=NULL, ...) {
                fri <- .self$getFriendIDs(n, ...)
                lookupUsers(fri, ...)
              },
              getFavouritesCount = function() {
                return(favoritesCount)
              },
              getFavorites = function(n=20, max_id=NULL, since_id=NULL, ...) {
                return(favorites(screenName, n=n, max_id=max_id, since_id=since_id, ...))
              },
              toDataFrame = function(row.names=NULL, optional=FALSE, stringsAsFactors=FALSE) {
                callSuper(row.names=row.names, optional=optional, stringsAsFactors=stringsAsFactors, 
                          fieldsToRemove='lastStatus')
              }
            )
          )

userFactory <- getRefClass("user")
userFactory$accessors(names(userFactory$fields()))

buildUser <- function(json)
  userFactory$new(json)


setMethod("show", signature="user", function(object) {
    print(screenName(object))
})



#' Functions to manage Twitter users
#' 
#' These functions allow you interact with information about a Twitter user -
#' retrieving their base information, list of friends, list of followers, and
#' an up to date timeline.
#' 
#' These functions will only return fully formed objects if the authenticated
#' user is allowed to see the requested user.  If that person has a private
#' account and has not allowed you to see them, you will not be able to extract
#' that information.
#' 
#' The \code{lookupUsers} function should be used in cases where there are
#' multiple lookups going to take place, to reduce the API call load.  This
#' function requires OAuth authentication.
#' 
#' @aliases getUser lookupUsers
#' @param user The Twitter user to detail, can be \code{character} or an
#' \code{\link{user}} object.
#' @param users A vector of either user IDs or screen names or a mix of both
#' @param includeNA If \code{TRUE} will leave an NA element in the return list
#' for users that don't exist
#' @param ... Optional arguments to be passed to \code{\link{GET}}
#' @return The \code{getUser} function returns an object of class
#' \code{\link{user}}.
#' 
#' The \code{lookupUsers} function will return a list of \code{\link{user}}
#' objects, sorted in the order of the \code{users} argument, with names being
#' the particular element of \code{users} that it matches to. If the
#' \code{includeNA} argument is set to \code{FALSE} (default), any non-existing
#' users will be dropped from the list.
#' @author Jeff Gentry
#' @seealso \code{\link{mentions}}
#' @keywords interface
#' @examples
#' 
#'       \dontrun{
#'         tuser <- getUser('geoffjentry')
#'         users <- lookupUsers(c('geoffjentry', 'whitehouse'))
#'       }
#' 
getUser <- function(user, ...) {
  params <- parseUsers(user)
  buildUser(twInterfaceObj$doAPICall(paste('users', 'show', sep='/'),
                                     params=params, ...))
}

lookupUsers <- function(users, includeNA=FALSE, ...) {
  MatchLookedUpUsers <- function(vals) {
    order <- match(tolower(users), tolower(vals))
    na.eles <- which(is.na(order))
    
    if (length(na.eles) > 0) {
      if (!includeNA) {
        order <- order[-na.eles]
        users <- users[-na.eles]
      }
    }
    out <- out[order]
    names(out) <- users
    
    return(out)
  }
  
  if (is.null(users) || length(users) == 0) {
    return(list())
  }
  
  batches <- split(users, ceiling(seq_along(users) / 100))
  results <- lapply(batches, function(batch) {
    params <- parseUsers(batch)
    twInterfaceObj$doAPICall(paste('users', 'lookup', sep='/'),
                             params=params, ...)
  })
  out <- sapply(do.call(c, results), buildUser)

  ## Order these to match the users vector - if !includeNA,
  ## drop out the elements of the return list which weren't
  ## found
  sn.lookups <- MatchLookedUpUsers(sapply(out,
                                          function(x) x$getScreenName()))
  id.lookups <- MatchLookedUpUsers(sapply(out, function(x) x$getId()))

  ## The problem with doing it in the two batch way above is that
  ## anything that was SN will be NULL for ID and vice versa.
  ## If includeNA is TRUE, we can't just remove all empty
  ## entries. As a hack, only retain the NULL values that are shared
  ## between both lists
  if (includeNA) {
    sn.nulls <- sapply(sn.lookups, is.null)
    id.nulls <- sapply(id.lookups, is.null)
    false.nulls <- xor(sn.nulls, id.nulls)
    sn.lookups <- sn.lookups[!(sn.nulls & false.nulls)]
    id.lookups <- id.lookups[!(id.nulls & false.nulls)]
  } else {
    ## Otherwise, just strip out the names that have been
    ## taken out
    users <- intersect(users, union(names(sn.lookups), names(id.lookups)))
  }
  
  out <- c(sn.lookups, id.lookups)
  return(out[users])
}

 
