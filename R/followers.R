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
