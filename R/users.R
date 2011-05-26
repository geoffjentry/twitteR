getUser <- function(user, ...) {
    if (inherits(user, "user"))
        user <- screenName(user)

    if (is.numeric(user))
      params <- list(user_id=user)
    else
      params <- list(screen_name=user)
    
    buildUser(twInterfaceObj$doAPICall(paste('users', 'show', sep='/'),
                                       params=params, ...))
}

lookupUsers <- function(users, ...) {
  users <- sapply(users, function(x) {
    if (inherits(x, 'user'))
      screenName(x)
  })

  batches <- split(z, ceiling(seq_along(z) / 100))
  results <- sapply(batches, function(batch, users) {
    curUsers <- users[batch]
    uids <- which(is.numeric(users))
    params[['user_id']] <- paste(curUsers[uids], collapse=',')
    params[['screen_name']] <- paste(curUsers[setdiff(curUsers,
                                                      params[['user_id']])],
                                     collapse=',')
    twInterfaceObj$doAPICall(paste('users', 'lookup', sep='/'),
                             params=params, ...)
  }, users)
  sapply(do.call(c, results), buildUser)
}
