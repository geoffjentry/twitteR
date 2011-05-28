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
    else
      x
  })

  batches <- split(users, ceiling(seq_along(users) / 100))
  results <- lapply(batches, function(batch, users) {
    uids <- which(!is.na(suppressWarnings(as.numeric(batch))))
    userIDs <- batch[uids]
    params <- list(user_id=paste(userIDs, collapse=','),
                   screen_name=paste(batch[setdiff(batch, uids)], collapse=','))
    twInterfaceObj$doAPICall(paste('users', 'lookup', sep='/'),
                             params=params, ...)
  }, users)
  sapply(do.call(c, results), buildUser)
}
