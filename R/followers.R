followers <- function(user, n=NULL, ...) {
  ffBase('followers', user, n, ...)
}

friends <- function(user, n=NULL, ...) {
  ffBase('friends', user, n, ...)
}
  

ffBase <- function(type, user, n=NULL, ...) {
  if (inherits(user, "user"))
    user <- screenName(user)
  
  if (is.numeric(user))
    params <- list(user_id=user)
  else
    params <- list(screen_name=user)
  twCursorInterfaceObj$doAPICall(paste(type, 'ids', sep='/'), 'ids', num=n, params=params, method='GET', ...)
}
