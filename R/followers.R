followers <- function(user, n=NULL, ...) {
  ffBase('followers', user, n, ...)
}

friends <- function(user, n=NULL, ...) {
  ffBase('friends', user, n, ...)
}

ffBase <- function(type, user, n=NULL, ...) {
  params <- parseUsers(user)
  
  doCursorAPICall(paste(type, 'ids', sep='/'), 'ids', num=n, params=params, method='GET', ...)
}



#' A function to detail relations between yourself & other users
#' 
#' This function will accept a list of other Twitter users and will detail if
#' they follow you and/or you follow them.
#' 
#' The combined number of screen names and user ids may not exceed 100. Any
#' non-existent users will be dropped from the output
#' 
#' @param screen_names A vector of one or more Twitter screen names
#' @param user_ids A vector of one or more Twitter user id values
#' @param \dots Any other arguments to pass to RCurl
#' @return A data.frame, one row for each user requested with columns
#' \code{name}, \code{screen_name}, \code{id}, \code{following} and
#' \code{followed_by}. The latter two columns will be \code{TRUE} or
#' \code{FALSE} depending on that user's relations with your account.
#' @author Jeff Gentry
#' @seealso \code{\link{registerTwitterOAuth}}
#' @references https://dev.twitter.com/docs/api/1.1/get/friendships/lookup
#' @keywords interface
#' @examples
#' 
#'   \dontrun{
#'     friendships()
#'   }
#' 
friendships = function(screen_names=character(), user_ids=character(), ...) {
  if ((length(user_ids) + length(screen_names)) > 100) {
    stop("friendships() has a maximum of 100 total user ids and screen names")
  }
  
  user_list = buildUserList(user_ids, screen_names)
  
  friendships = twInterfaceObj$doAPICall("friendships/lookup", params=user_list, ...)
  friendship_dfs = lapply(friendships, function(x) {
    following = "following" %in% x$connections
    followed_by = "followed_by" %in% x$connections
    return(c(name=x$name, screen_name=x$screen_name, id=x$id_str, following=following, 
             followed_by=followed_by))    
  })
  
  return(as.data.frame(do.call(rbind, friendship_dfs), stringsAsFactors=FALSE))
}
