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