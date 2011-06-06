getUser <- function(user, ...) {
  params <- parseUser(user)
  buildUser(twInterfaceObj$doAPICall(paste('users', 'show', sep='/'),
                                     params=params, ...))
}

lookupUsers <- function(users, ...) {
  batches <- split(users, ceiling(seq_along(users) / 100))
  results <- lapply(batches, function(batch) {
    params <- parseUsers(users)
    twInterfaceObj$doAPICall(paste('users', 'lookup', sep='/'),
                             params=params, ...)
  })
  sapply(do.call(c, results), buildUser)
}
